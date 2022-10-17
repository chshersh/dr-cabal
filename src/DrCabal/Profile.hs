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
    , getTerminalWidth
    , createChart
    ) where

import Colourista.Short (u)
import Data.Aeson (eitherDecodeFileStrict')
import System.Console.Terminal.Size (Window(..), size)

import DrCabal.Cli (ProfileArgs (..))
import DrCabal.Model (Entry (..), Style (Stacked))
import DrCabal.Profile.Stacked (createStackedChart)

import qualified Colourista

getTerminalWidth :: IO Int
getTerminalWidth = size >>= \case
    Just (Window _height width) -> pure width
    Nothing -> do
        putText $ unlines
            [ "Error getting the terminal width. If you see this error, open an issue"
            , "in the 'dr-cabal' issue tracker and provide as many details as possible"
            , ""
            , "  * " <> u "https://github.com/chshersh/dr-cabal/issues/new"
            ]
        exitFailure

runProfile :: ProfileArgs -> IO ()
runProfile ProfileArgs{..} = do
    terminalWidth <- getTerminalWidth

    entries <- readFromFile profileArgsInput

    let chart = createChart profileArgsStyle terminalWidth entries

    putTextLn chart

createChart :: Style -> Int -> [Entry] -> Text
createChart = \case
    Stacked -> createStackedChart

readFromFile :: FilePath -> IO [Entry]
readFromFile file = eitherDecodeFileStrict' file >>= \case
    Left err -> do
        Colourista.errorMessage $ "Error parsing file: " <> toText file
        Colourista.redMessage   $ "      " <> toText err
        exitFailure
    Right entries -> pure entries
