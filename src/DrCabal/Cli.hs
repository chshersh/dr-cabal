{-# LANGUAGE ApplicativeDo #-}

{- |
Copyright: (c) 2022 Dmitrii Kovanikov
SPDX-License-Identifier: MPL-2.0
Maintainer: Dmitrii Kovanikov <kovanikov@gmail.com>

CLI parser for @dr-cabal@.
-}

module DrCabal.Cli
    ( Args (..)
    , readArgs
    ) where

import qualified Options.Applicative as Opt

data Args = Args
    { argsSaveLogs :: Maybe FilePath
    , argsFromFile :: Maybe FilePath
    }

readArgs :: IO Args
readArgs = Opt.execParser opts
  where
    opts = Opt.info (Opt.helper <*> argsP) $ mconcat
      [ Opt.fullDesc
      , Opt.progDesc "Profile cabal dependency build output"
      , Opt.header "dr-cabal - a CLI tool to treat cabal output"
      ]

argsP :: Opt.Parser Args
argsP = do
    argsSaveLogs <- Opt.optional $ Opt.strOption $ mconcat
        [ Opt.long "save-logs"
        , Opt.metavar "FILE_PATH"
        , Opt.help "Save intermediate logs of cabal output to a JSON file"
        ]

    argsFromFile <- Opt.optional $ Opt.strOption $ mconcat
        [ Opt.long "from-file"
        , Opt.metavar "FILE_PATH"
        , Opt.help "Profile cabal output from a file saved by --save-logs instead of stdin"
        ]

    pure Args{..}
