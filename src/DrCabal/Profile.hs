{- |
Module                  : DrCabal.Profile
Copyright               : (c) 2022 Dmitrii Kovanikov
SPDX-License-Identifier : MPL-2.0
Maintainer              : Dmitrii Kovanikov <kovanikov@gmail.com>
Stability               : Experimental
Portability             : Portable

@dr-cabal profile@ command.
-}

module DrCabal.Profile
    ( runProfile
    ) where

import Colourista.Short (u)
import Data.Aeson (eitherDecodeFileStrict')
import System.Console.ANSI (getTerminalSize)

import DrCabal.Cli (ProfileArgs (..))
import DrCabal.Model (Entry (..), Style (Stacked))
import DrCabal.Profile.Stacked (createStackedChart)

import qualified Colourista

runProfile :: ProfileArgs -> IO ()
runProfile ProfileArgs{..} = do
    terminalWidth <- getTerminalSize >>= \case
        Just (_height, width) -> pure width
        Nothing -> do
            putText $ unlines
                [ "Error getting the terminal width. If you see this error, open an issue"
                , "in the 'dr-cabal' issue tracker and provide as many details as possible"
                , ""
                , "  * " <> u "https://github.com/chshersh/dr-cabal/issues/new"
                ]
            exitFailure

    entries <- readFromFile profileArgsInput

    let chart = case profileArgsStyle of
          Stacked -> createStackedChart terminalWidth entries

    putTextLn chart

readFromFile :: FilePath -> IO [Entry]
readFromFile file = eitherDecodeFileStrict' file >>= \case
    Left err -> do
        Colourista.errorMessage $ "Error parsing file: " <> toText file
        Colourista.redMessage   $ "      " <> toText err
        exitFailure
    Right entries -> pure entries
