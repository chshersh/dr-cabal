{- |
Copyright: (c) 2022 Dmitrii Kovanikov
SPDX-License-Identifier: MPL-2.0
Maintainer: Dmitrii Kovanikov <kovanikov@gmail.com>

Implementation of the 'dr-cabal watch' command.
-}

module DrCabal.Watch
    ( watch
    ) where

import Data.Aeson.Encode.Pretty (encodePretty)
import GHC.Clock (getMonotonicTimeNSec)
import System.IO (isEOF)

import DrCabal.Cli (WatchArgs (..))
import DrCabal.Model (Entry (..), Line (..))

import qualified Data.ByteString as ByteString
import qualified Data.Text as Text


watch :: WatchArgs -> IO ()
watch WatchArgs{..} = do
    putTextLn "Collecting stats... This operation may take a while..."

    cabalOutput <- readFromStdin
    writeFileLBS watchArgsOutput $ encodePretty cabalOutput

    putTextLn $ "Cabal output cached in a file: " <> toText watchArgsOutput

readFromStdin :: IO [Entry]
readFromStdin = go []
  where
    go :: [Line] -> IO [Entry]
    go cabalOutput = do
        isEndOfInput <- isEOF
        if isEndOfInput
        then pure $ linesToEntries cabalOutput
        else do
            time <- getMonotonicTimeNSec
            line <- ByteString.getLine
            go $ Line time line : cabalOutput

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
