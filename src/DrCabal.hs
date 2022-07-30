{- |
Copyright: (c) 2022 Dmitrii Kovanikov
SPDX-License-Identifier: MPL-2.0
Maintainer: Dmitrii Kovanikov <kovanikov@gmail.com>

See README for more info
-}

module DrCabal
    ( main
    ) where

import Data.Aeson (eitherDecodeFileStrict')
import Data.Aeson.Encode.Pretty (encodePretty)
import GHC.Clock (getMonotonicTimeNSec)
import System.IO (isEOF)

import DrCabal.Cli (Args (..), readArgs)
import DrCabal.Model (Entry (..), Line (..), Status (..))

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as Text


main :: IO ()
main = readArgs >>= runDrCabal

runDrCabal :: Args -> IO ()
runDrCabal Args{..} = do
    cabalOutput <- case argsFromFile of
        Nothing   -> readFromStdin argsSaveLogs
        Just file -> readFromFile file

    pure ()

readFromStdin :: Maybe FilePath -> IO [Entry]
readFromStdin mSaveLogsFile = go []
  where
    go :: [Line] -> IO [Entry]
    go cabalOutput = do
        isEndOfInput <- isEOF
        if isEndOfInput
        then do
            let entries = linesToEntries cabalOutput

            whenJust mSaveLogsFile $ \saveLogsFile ->
                writeFileLBS saveLogsFile $ encodePretty entries

            pure entries
        else do
            time <- getMonotonicTimeNSec
            line <- ByteString.getLine
            BS8.putStrLn $ show time <> "  >>>  " <> line
            go $ Line time line : cabalOutput

readFromFile :: FilePath -> IO [Entry]
readFromFile file = eitherDecodeFileStrict' file >>= \case
    Left err -> do
        putStrLn $ "Error parsing file: " <> file
        putStrLn $ "    " <> err
        exitFailure
    Right entries -> pure entries

linesToEntries :: [Line] -> [Entry]
linesToEntries = mapMaybe parseLine . reverse

parseLine :: Line -> Maybe Entry
parseLine Line{..} = do
    let txtLine = decodeUtf8 lineLine
    txtStatus : library : _ <- Just $ words txtLine

    -- parse status string to the 'Status' type
    status <- readMaybe $ toString txtStatus

    -- check if this line is a library: '-' separates library name and its version
    guard $ Text.elem '-' library

    pure $ Entry
        { entryStatus  = status
        , entryStart   = lineTime
        , entryLibrary = library
        }
