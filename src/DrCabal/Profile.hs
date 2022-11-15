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
    ) where


import DrCabal.Cli (FileMode (..), ProfileArgs (..))
import DrCabal.Json (readEntries, writeEntries)
import DrCabal.Model (Entry (..), Style (..))
import DrCabal.Profile.Stacked (createStackedChart)
import DrCabal.Terminal (getTerminalWidth)
import DrCabal.Watch (watchBuild)


runProfile :: ProfileArgs -> IO ()
runProfile ProfileArgs{..} = case profileArgsFileMode of
     None ->
         profileInteractive profileArgsStyle
     Output outputFile ->
         profileWithOutput profileArgsStyle outputFile
     Input inputFile ->
         profileFromInput profileArgsStyle inputFile

profileInteractive :: Style -> IO ()
profileInteractive chartStyle = do
    hSetBuffering stdout (BlockBuffering Nothing)
    terminalWidth <- getTerminalWidth
    let drawChart = createChart chartStyle terminalWidth
    void $ watchBuild drawChart

profileWithOutput :: Style -> FilePath -> IO ()
profileWithOutput chartStyle outputFile = do
    hSetBuffering stdout (BlockBuffering Nothing)
    terminalWidth <- getTerminalWidth
    let drawChart = createChart chartStyle terminalWidth
    entries <- watchBuild drawChart
    writeEntries outputFile entries

profileFromInput :: Style -> FilePath -> IO ()
profileFromInput chartStyle inputFile = do
    terminalWidth <- getTerminalWidth
    entries <- readEntries inputFile
    let chart = createChart chartStyle terminalWidth entries
    putTextLn chart

createChart
    :: Style
    -- ^ Chart type
    -> Int
    -- ^ Terminal width
    -> [Entry]
    -- ^ Time entries
    -> Text
createChart = \case
    Stacked -> createStackedChart
