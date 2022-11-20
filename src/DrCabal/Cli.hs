{-# LANGUAGE ApplicativeDo #-}

{- |
Module                  : DrCabal.Cli
Copyright               : (c) 2022 Dmitrii Kovanikov
SPDX-License-Identifier : MPL-2.0
Maintainer              : Dmitrii Kovanikov <kovanikov@gmail.com>
Stability               : Experimental
Portability             : Portable

CLI parser for @dr-cabal@.
-}

module DrCabal.Cli
    ( Command (..)
    , readCommand

    , ProfileArgs (..)
    , FileMode (..)
    ) where

import DrCabal.Model (Style (..))

import qualified Options.Applicative as Opt

data Command
    = Profile ProfileArgs

data ProfileArgs = ProfileArgs
    { profileArgsStyle    :: Style
    , profileArgsFileMode :: FileMode
    }

data FileMode
    -- | Don't read from the file and don't store the results in the file
    = None

    -- | Store current results in the file
    | Output FilePath

    -- | Read previously saved results from the file
    | Input FilePath

readCommand :: IO Command
readCommand = Opt.execParser opts
  where
    opts = Opt.info (Opt.helper <*> commandP) $ mconcat
      [ Opt.fullDesc
      , Opt.progDesc "Profile cabal dependency build output"
      , Opt.header "dr-cabal - a CLI tool to treat cabal output"
      ]

-- | All possible commands.
commandP :: Opt.Parser Command
commandP = Opt.subparser $ mconcat
    [ Opt.command "profile"
        $ Opt.info (Opt.helper <*> profileP)
        $ Opt.progDesc "Build profiling report"
    ]

profileP :: Opt.Parser Command
profileP = do
    profileArgsStyle <- styleP
    profileArgsFileMode <- fileModeP

    pure $ Profile ProfileArgs{..}

styleP :: Opt.Parser Style
styleP = stackedP <|> pure Stacked
  where
    stackedP :: Opt.Parser Style
    stackedP = Opt.flag' Stacked $ mconcat
        [ Opt.long "stacked"
        , Opt.short 's'
        , Opt.help "Format as stacked"
        ]

fileModeP :: Opt.Parser FileMode
fileModeP = inputP <|> outputP <|> pure None
  where
    inputP :: Opt.Parser FileMode
    inputP = fmap Input $ Opt.strOption $ mconcat
        [ Opt.long "input"
        , Opt.short 'i'
        , Opt.metavar "FILE_PATH"
        , Opt.help "Read profile input from a JSON file, created by 'dr-cabal profile --output=<some-file>'"
        ]

    outputP :: Opt.Parser FileMode
    outputP = fmap Output $ Opt.strOption $ mconcat
        [ Opt.long "output"
        , Opt.short 'o'
        , Opt.metavar "FILE_PATH"
        , Opt.help "Save cabal output to a file in a JSON format"
        ]
