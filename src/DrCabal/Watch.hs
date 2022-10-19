{-# LANGUAGE NumericUnderscores #-}

{- |
Module                  : DrCabal.Watch
Copyright               : (c) 2019 Alexander Gugel
                          (c) 2022 Dmitrii Kovanikov
SPDX-License-Identifier : MPL-2.0
Maintainer              : Dmitrii Kovanikov <kovanikov@gmail.com>
Stability               : Experimental
Portability             : Portable

Implementation of the @dr-cabal watch@ command.
-}

module DrCabal.Watch
    ( runWatch
    , parseLine
    ) where

import Colourista.Short (b)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently_)
import Data.Aeson.Encode.Pretty (encodePretty)
import GHC.Clock (getMonotonicTimeNSec)
import System.Console.ANSI (clearLine, setCursorColumn)
import System.IO (isEOF)

import DrCabal.Cli (WatchArgs (..))
import DrCabal.Model (Entry (..), Line (..))

import qualified Colourista
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text


runWatch :: WatchArgs -> IO ()
runWatch WatchArgs{..} = do
    watchRef <- newIORef [Start]

    concurrently_
        (watchWorker watchRef)
        (readFromStdin watchRef watchArgsOutput)

readFromStdin :: IORef [WatchAction] -> FilePath -> IO ()
readFromStdin watchRef outputPath = go []
  where
    go :: [Line] -> IO ()
    go cabalOutput = do
        isEndOfInput <- isEOF
        if isEndOfInput
        then do
            pushAction watchRef $ End outputPath cabalOutput
        else do
            time <- getMonotonicTimeNSec
            line <- ByteString.getLine

            -- output line to the watch worker for output redirection
            pushAction watchRef $ Consume line

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

data WatchAction
    = Start
    | Consume ByteString
    | End FilePath [Line]

-- | Add 'WatchAction' to end of the list
pushAction :: IORef [WatchAction] -> WatchAction -> IO ()
pushAction watchRef action =
    atomicModifyIORef' watchRef $ \actions -> (actions ++ [action], ())

data WorkerCommand
    = Greeting
    | WriteLine ByteString
    | Wait
    | Finish FilePath [Line]

watchWorker :: IORef [WatchAction] -> IO ()
watchWorker watchRef = go "Watching build output" (cycle spinnerFrames)
  where
    spinnerFrames :: [Text]
    spinnerFrames =
        [ "⠋"
        , "⠙"
        , "⠹"
        , "⠸"
        , "⠼"
        , "⠴"
        , "⠦"
        , "⠧"
        , "⠇"
        , "⠏"
        ]

    go :: Text -> [Text] -> IO ()
    go _ [] = do
        Colourista.errorMessage $
            "Panic! At the 'dr-cabal'! Impossible happened: list of frames is empty"
        exitFailure
    go prevLine (frame : frames) = do
        command <- atomicModifyIORef' watchRef popAction
        case command of
            Greeting -> do
                Colourista.formattedMessage
                    [Colourista.blue, Colourista.bold]
                    "Watching cabal output..."

                go prevLine (frame : frames)
            WriteLine line -> do
                resetLine
                let l = decodeUtf8 line
                putText $ frame <> " " <> l
                hFlush stdout
                threadDelay 80_000  -- wait 80 ms to update spinner
                go l frames
            Wait -> do
                resetLine
                putText $ frame <> " " <> prevLine
                hFlush stdout
                threadDelay 80_000  -- wait 80 ms to update spinner
                go prevLine frames
            Finish outputPath lns -> do
                writeFileLBS outputPath $ encodePretty $ linesToEntries lns
                resetLine
                putTextLn $ unlines
                    [ b "Build finished successfully!"
                    , ""
                    , "To see the profiling output, run the following command:"
                    , ""
                    , "    dr-cabal profile --input=" <> toText outputPath
                    ]

    popAction :: [WatchAction] -> ([WatchAction], WorkerCommand)
    popAction [] = ([], Wait)
    popAction (x : xs) = case x of
        Start        -> (xs, Greeting)
        Consume l    -> (xs, WriteLine l)
        End path lns -> ([], Finish path lns)

    resetLine :: IO ()
    resetLine = do
        clearLine
        setCursorColumn 0
