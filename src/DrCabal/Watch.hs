{-# LANGUAGE NumericUnderscores #-}

{- |
Module                  : DrCabal.Watch
Copyright               : (c) 2019 Alexander Gugel
                          (c) 2022 Dmitrii Kovanikov
SPDX-License-Identifier : MPL-2.0
Maintainer              : Dmitrii Kovanikov <kovanikov@gmail.com>
Stability               : Experimental
Portability             : Portable

Watch the output of the @cabal build@ command and update the profile
chart interactively.
-}

module DrCabal.Watch
    ( watchBuild
    ) where

import Colourista.Short (b)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently)
import GHC.Clock (getMonotonicTimeNSec)
import System.IO (isEOF)

import DrCabal.Model (Entry (..), Line (..), parseLine)
import DrCabal.Terminal (clearScreen)

import qualified Colourista
import qualified Data.ByteString as ByteString


{- | Watch build entries from @stdin@ and interactively update the
chart and current status.
-}
watchBuild
    :: ([Entry] -> Text)
    -- ^ A function to draw chart
    -> IO [Entry]
    -- ^ Returns the final list of entries
watchBuild drawChart = do
    inputActionRef <- newIORef [Start]

    (entries, _) <- concurrently
        (interactiveWorker inputActionRef drawChart)
        (stdinReaderWorker inputActionRef)

    pure entries

stdinReaderWorker :: IORef [InputAction] -> IO ()
stdinReaderWorker inputActionRef = go
  where
    go :: IO ()
    go = do
        isEndOfInput <- isEOF
        if isEndOfInput
        then do
            pushAction inputActionRef End
        else do
            time <- getMonotonicTimeNSec
            line <- ByteString.getLine
            let ln = Line time line

            -- output line to the watch worker for output redirection
            pushAction inputActionRef $ Consume ln

            go

-- | Action returned by the @stdinReaderWorker@.
data InputAction
    -- | Produce the initial message
    = Start

    -- | Line content read from @stdin@ with timestamp
    | Consume Line

    -- | EOF reached for @stdin@
    | End

-- | Add 'InputAction' to end of the queue in the given 'IORef'.
pushAction :: IORef [InputAction] -> InputAction -> IO ()
pushAction inputActionRef action =
    atomicModifyIORef' inputActionRef $ \actions -> (actions ++ [action], ())

data InteractiveCommand
    -- | Initial message
    = Greeting

    -- | New line received from the @stdinReaderWorker@. Update the chart.
    | UpdateChart Line

    -- | No new lines from @stdin@. Simply wait and update the spinner.
    | Wait

    -- | Finished reading lines from @stdin@
    | Finish

{- | Produce the next 'InteractiveCommand' by reading the current
'InputAction' and removing it from the queue.
-}
nextCommand :: IORef [InputAction] -> IO InteractiveCommand
nextCommand inputActionRef = atomicModifyIORef' inputActionRef popAction
  where
    popAction :: [InputAction] -> ([InputAction], InteractiveCommand)
    popAction [] = ([], Wait)
    popAction (x : xs) = case x of
        Start     -> (xs, Greeting)
        Consume l -> (xs, UpdateChart l)
        End       -> ([], Finish)

-- | A data type
data Output = Output
    { outputCabalLog :: Text
    , outputEntries  :: [Entry]
    }

interactiveWorker
    :: IORef [InputAction]
    -- ^ Mutable reference to the queue of input actions
    -> ([Entry] -> Text)
    -- ^ A function to draw the chart
    -> IO [Entry]
interactiveWorker inputActionRef drawChart =
    go (Output "Profiling 'cabal build' interactively..." []) (cycle spinnerFrames)
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

    go :: Output -> [Text] -> IO [Entry]
    go _ [] = do
        Colourista.errorMessage $
            "Panic! At the 'dr-cabal'! Impossible happened: list of frames is empty"
        exitFailure
    go prevOutput (frame : frames) = do
        command <- nextCommand inputActionRef

        case command of
            Greeting -> do
                printOutput frame prevOutput prevOutput
                go prevOutput (frame : frames)
            UpdateChart line@Line{..} -> case parseLine line of
                Nothing -> do
                    let newOutput = prevOutput { outputCabalLog = decodeUtf8 lineLine }
                    printOutput frame prevOutput newOutput
                    go newOutput frames
                Just entry -> do
                    let newOutput = Output
                            { outputCabalLog = decodeUtf8 lineLine
                            , outputEntries  = outputEntries prevOutput ++ [entry]
                            }
                    printOutput frame prevOutput newOutput
                    go newOutput frames
            Wait -> do
                printOutput frame prevOutput prevOutput
                go prevOutput frames

            Finish -> do
                putTextLn $ b "Build finished successfully!"
                pure $ outputEntries prevOutput

    printOutput :: Text -> Output -> Output -> IO ()
    printOutput frame oldOutput newOutput = do
        clearPreviousOutput oldOutput
        putText $ fmtOutput frame newOutput
        hFlush stdout
        threadDelay 80_000  -- wait 80 ms to update spinner

    clearPreviousOutput :: Output -> IO ()
    clearPreviousOutput output = do
        let fakeFrame = ""
        let screenHeight = length $ lines $ fmtOutput fakeFrame output
        clearScreen screenHeight

    fmtOutput :: Text -> Output -> Text
    fmtOutput frame Output{..} =
        chart <> log
      where
        chart :: Text
        chart = case outputEntries of
            [] -> ""
            _  -> drawChart outputEntries

        log :: Text
        log = frame <> " " <> outputCabalLog
