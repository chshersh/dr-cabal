{-# LANGUAGE NumericUnderscores #-}

{- |
Module                  : DrCabal.Interactive
Copyright               : (c) 2022 Dmitrii Kovanikov
                          (c) 2022 Andrew Lelechenko
SPDX-License-Identifier : MPL-2.0
Maintainer              : Dmitrii Kovanikov <kovanikov@gmail.com>
Stability               : Experimental
Portability             : Portable

Implementation of the @dr-cabal@ interactive command.
-}

module DrCabal.Interactive
    ( runInteractive
    ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently_)
import GHC.Clock (getMonotonicTimeNSec)
import System.Console.ANSI (clearFromCursorToScreenEnd, cursorUpLine)
import System.IO (isEOF)

import DrCabal.Cli (InteractiveArgs (..))
import DrCabal.Model (Entry (..), Line (..), Style)
import DrCabal.Profile (getTerminalWidth, createChart)
import DrCabal.Watch (parseLine)

import qualified Data.ByteString as ByteString


runInteractive :: InteractiveArgs -> IO ()
runInteractive InteractiveArgs{..} = do
    hSetBuffering stdout (BlockBuffering Nothing)
    terminalWidth <- getTerminalWidth

    stateRef <- newIORef (InteractiveState [] False)

    concurrently_
        (watchWorker interactiveArgsStyle terminalWidth 0 stateRef)
        (readFromStdin stateRef)

data InteractiveState = InteractiveState
    { istEntries :: [Entry]
    , istDone    :: Bool
    }

readFromStdin :: IORef InteractiveState -> IO ()
readFromStdin stateRef = go
  where
    go :: IO ()
    go = do
        isEndOfInput <- isEOF
        if isEndOfInput
        then atomicModifyIORef' stateRef $ \st -> (st { istDone = True }, ())
        else do
            time <- getMonotonicTimeNSec
            line <- ByteString.getLine
            case parseLine (Line time line) of
                Nothing -> pure ()
                Just entry -> atomicModifyIORef' stateRef $
                    \st -> (st { istEntries = istEntries st ++ [entry] }, ())
            go

watchWorker :: Style -> Int -> Int -> IORef InteractiveState -> IO ()
watchWorker style terminalWidth chartHeight stateRef = do
    InteractiveState entries done <- readIORef stateRef

    clearScreen chartHeight

    let chart = case entries of
            [] -> ""
            _ -> createChart style terminalWidth entries
    putText chart
    hFlush stdout

    unless done $ do
        threadDelay 80_000  -- wait 80 ms to update
        let newChartHeight = length (lines chart)
        watchWorker style terminalWidth newChartHeight stateRef

clearScreen :: Int -> IO ()
clearScreen chartHeight = do
    -- https://github.com/UnkindPartition/ansi-terminal/issues/141
    when (chartHeight > 0) $
        cursorUpLine chartHeight
    clearFromCursorToScreenEnd
