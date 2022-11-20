{- |
Module                  : DrCabal.Json
Copyright               : (c) 2022 Dmitrii Kovanikov
SPDX-License-Identifier : MPL-2.0
Maintainer              : Dmitrii Kovanikov <kovanikov@gmail.com>
Stability               : Experimental
Portability             : Portable

This module contains functions to process JSON entries recognised by @dr-cabal@.
-}

module DrCabal.Json
    ( readEntries
    , writeEntries
    ) where

import Data.Aeson (eitherDecodeFileStrict')
import Data.Aeson.Encode.Pretty (encodePretty)

import DrCabal.Model (Entry)

import qualified Colourista


{- | Read a list of entries from a JSON file.

Exits process with an error message on decoding failure.
-}
readEntries :: FilePath -> IO [Entry]
readEntries file = eitherDecodeFileStrict' file >>= \case
    Right entries -> pure entries
    Left err -> do
        Colourista.errorMessage $ "Error parsing file: " <> toText file
        Colourista.redMessage   $ "      " <> toText err
        exitFailure

-- | Write entries as a pretty JSON to the output file.
writeEntries :: FilePath -> [Entry] -> IO ()
writeEntries outputPath = writeFileLBS outputPath . encodePretty
