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

    , WatchArgs (..)
    , ProfileArgs (..)
    ) where

import qualified Options.Applicative as Opt

data Command
    = Watch WatchArgs
    | Profile ProfileArgs

newtype WatchArgs = WatchArgs
    { watchArgsOutput :: FilePath
    }

newtype ProfileArgs = ProfileArgs
    { profileArgsInput :: FilePath
    }

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
    [ Opt.command "watch"
          $ Opt.info (Opt.helper <*> watchP)
          $ Opt.progDesc "Watch cabal output and save it"
    , Opt.command "profile"
          $ Opt.info (Opt.helper <*> profileP)
          $ Opt.progDesc "Output pretty cabal profile results"
    ]

watchP :: Opt.Parser Command
watchP = do
    watchArgsOutput <- Opt.strOption $ mconcat
        [ Opt.long "output"
        , Opt.short 'o'
        , Opt.metavar "FILE_PATH"
        , Opt.help "Save cabal output to a file in a JSON format"
        ]

    pure $ Watch WatchArgs{..}

profileP :: Opt.Parser Command
profileP = do
    profileArgsInput <-  Opt.strOption $ mconcat
        [ Opt.long "input"
        , Opt.short 'i'
        , Opt.metavar "FILE_PATH"
        , Opt.help "Read profile input from a JSON file, created by 'dr-cabal watch'"
        ]

    pure $ Profile ProfileArgs{..}
