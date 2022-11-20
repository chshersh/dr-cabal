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
import DrCabal.Terminal (getTerminalWidth, withAlternateBuffer)
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
    -- draw profiling chart interactively and get all the entries after that
    entries <- withInteractiveProfiling chartStyle

    -- draw the chart in the normal terminal screen buffer now
    drawChart <- getChartDrawer chartStyle
    putTextLn $ drawChart entries

    putText $ unlines
        [ "✨  Done!"
        , "🆙  Scroll up to view full profiling chart."
        , "💾  To save the results in a file (to view later without recompilation), run:"
        , ""
        , "    cabal build ... | dr-cabal profile --output=my_file.json"
        ]

profileWithOutput :: Style -> FilePath -> IO ()
profileWithOutput chartStyle outputFile = do
    -- draw profiling chart interactively and get all the entries after that
    entries <- withInteractiveProfiling chartStyle
    writeEntries outputFile entries

    putText $ unlines
        [ "✨  Done!"
        , "💾  Profiling entries are saved in the file: " <> toText outputFile
        , "👀  To view the results from the saved file, run:"
        , ""
        , "    dr-cabal profile --input=" <> toText outputFile
        ]

profileFromInput :: Style -> FilePath -> IO ()
profileFromInput chartStyle inputFile = do
    terminalWidth <- getTerminalWidth
    entries <- readEntries inputFile
    let chart = createChart chartStyle terminalWidth entries
    putTextLn chart

-------------
-- HELPERS --
-------------

withInteractiveProfiling :: Style -> IO [Entry]
withInteractiveProfiling chartStyle = withAlternateBuffer $ do
    hSetBuffering stdout (BlockBuffering Nothing)
    drawChart <- getChartDrawer chartStyle
    watchBuild drawChart

getChartDrawer :: Style -> IO ([Entry] -> Text)
getChartDrawer chartStyle = do
    terminalWidth <- getTerminalWidth
    pure $ createChart chartStyle terminalWidth

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
