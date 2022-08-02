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

import Colourista.Pure (blue, cyan, formatWith, red, yellow, magenta)
import Colourista.Short (b, i, u)
import Data.Aeson (eitherDecodeFileStrict')
import System.Console.ANSI (getTerminalSize)

import DrCabal.Cli (ProfileArgs (..))
import DrCabal.Model (Entry (..), Status (..))

import qualified Colourista
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text


runProfile :: ProfileArgs -> IO ()
runProfile ProfileArgs{..} = do
    terminalWidth <- getTerminalSize >>= \case
        Just (_height, width) -> pure width
        Nothing -> do
            putText $ unlines
                [ "Error getting the terminal width. If you see this error, open an issue"
                , "in the 'dr-cabal' issue tracker and provide as many details as possible"
                , ""
                , "  * " <> u "https://github.com/chshersh/dr-cabal/issues/new"
                ]
            exitFailure

    entries <- readFromFile profileArgsInput
    let chart = createProfileChart terminalWidth entries
    putTextLn chart

readFromFile :: FilePath -> IO [Entry]
readFromFile file = eitherDecodeFileStrict' file >>= \case
    Left err -> do
        Colourista.errorMessage $ "Error parsing file: " <> toText file
        Colourista.redMessage   $ "      " <> toText err
        exitFailure
    Right entries -> pure entries

createProfileChart :: Int -> [Entry] -> Text
createProfileChart width l = case l of
    [] -> unlines
        [ "No cabal build entries found. Have you already built dependency?"
        , "Try removing global cabal store cache and rerunning 'dr-cabal watch' again."
        ]
    entries ->
        let start = List.minimum $ map entryStart entries in
        let end   = List.maximum $ map entryStart entries in
        formatChart start end width $ calculatePhases start $ groupEntries entries

groupEntries :: [Entry] -> Map Text [(Status, Word64)]
groupEntries = foldl' insert mempty
  where
    insert :: Map Text [(Status, Word64)] -> Entry -> Map Text [(Status, Word64)]
    insert m Entry{..} = Map.alter (Just . toVal (entryStatus, entryStart)) entryLibrary m
      where
        toVal :: a -> Maybe [a] -> [a]
        toVal x Nothing   = [x]
        toVal x (Just xs) = x : xs

data Phase = Phase
    { phaseDownloading :: Word64
    , phaseStarting    :: Word64
    , phaseBuilding    :: Word64
    , phaseHaddock     :: Word64
    , phaseInstalling  :: Word64
    }

phaseTotal :: Phase -> Word64
phaseTotal (Phase p1 p2 p3 p4 p5) = p1 + p2 + p3 + p4 + p5

calculatePhases :: Word64 -> Map Text [(Status, Word64)] -> Map Text Phase
calculatePhases start = fmap (entriesToPhase start)

entriesToPhase :: Word64 -> [(Status, Word64)] -> Phase
entriesToPhase start times = Phase
    { phaseDownloading = calcDownloading
    , phaseStarting    = calcStarting
    , phaseBuilding    = calcBuilding
    , phaseHaddock     = calcHaddock
    , phaseInstalling  = calcInstalling
    }
  where
    downloading, downloaded, starting, building, haddock, installing, completed :: Maybe Word64
    downloading = List.lookup Downloading times
    downloaded  = List.lookup Downloaded  times
    starting    = List.lookup Starting    times
    building    = List.lookup Building    times
    haddock     = List.lookup Haddock     times
    installing  = List.lookup Installing  times
    completed   = List.lookup Completed   times

    minusw :: Word64 -> Word64 -> Word64
    x `minusw` y
        | x <= y    = 0
        | otherwise = x - y

    calcDownloading :: Word64
    calcDownloading = case (downloading, downloaded) of
        (Just dStart, Just dEnd) -> dEnd `minusw` dStart
        _                        -> 0

    calcStarting :: Word64
    calcStarting = case building of
        Nothing -> 0
        Just bt -> case starting of
            Just st -> bt `minusw` st
            Nothing -> bt `minusw` start

    calcBuilding :: Word64
    calcBuilding = case haddock <|> installing of
      Nothing -> 0
      Just ba -> case building of
        Nothing -> ba `minusw` start
        Just bt -> ba `minusw` bt

    calcHaddock :: Word64
    calcHaddock = case haddock of
        Nothing -> 0
        Just hd -> case installing of
            Nothing -> hd `minusw` start
            Just it -> it `minusw` hd

    calcInstalling :: Word64
    calcInstalling = case completed of
        Nothing -> 0
        Just ct -> case installing of
            Nothing -> ct `minusw` start
            Just it -> ct `minusw` it

formatChart :: Word64 -> Word64 -> Int -> Map Text Phase -> Text
formatChart start end width libs = unlines $ concat $
    [ legend
    , summary
    , profile
    ]
  where
    block :: Text
    block = "▇"

    legend :: [Text]
    legend =
        [ b "Legend"
        , "  " <> fmt [cyan]    block <> "  Downloading"
        , "  " <> fmt [blue]    block <> "  Starting"
        , "  " <> fmt [red]     block <> "  Building"
        , "  " <> fmt [magenta] block <> "  Haddock"
        , "  " <> fmt [yellow]  block <> "  Installing"
        , ""
        ]

    summary :: [Text]
    summary =
        [ b "Summary"
        , i "  Total dependency build time" <> " : " <> fmtNanos (end - start)
        , i "  Single block resolution    " <> " : " <> fmtNanos blockMeasure
        , ""
        ]

    profile :: [Text]
    profile =
        [ b "Profile"
        ] ++
        formattedEntries

    formattedEntries :: [Text]
    formattedEntries
        = map (uncurry formatRow)
        $ sortOn (Down . phaseTotal . snd) entries

    formatRow :: Text -> Phase -> Text
    formatRow libName phase@Phase{..} = mconcat
        [ fmtPrefix libName phase
        , formatSinglePhase cyan    phaseDownloading
        , formatSinglePhase blue    phaseStarting
        , formatSinglePhase red     phaseBuilding
        , formatSinglePhase magenta phaseHaddock
        , formatSinglePhase yellow  phaseInstalling
        ]

    entries :: [(Text, Phase)]
    entries = Map.toList libs

    libSize, phaseSize, prefixSize :: Int
    libSize    = List.maximum $ map (Text.length . fst) entries
    phaseSize  = List.maximum $ map (Text.length . fmtPhase . snd) entries
    prefixSize = List.maximum $ map (Text.length . uncurry fmtPrefix) entries

    longestPhase :: Word64
    longestPhase = List.maximum $ map (phaseTotal . snd) entries

    fmtPhase :: Phase -> Text
    fmtPhase = fmtNanos . phaseTotal

    fmtPrefix :: Text -> Phase -> Text
    fmtPrefix libName phase = mconcat
        [ Text.justifyRight libSize ' ' libName
        , " ["
        , Text.justifyLeft phaseSize ' ' $ fmtPhase phase
        , "] "
        , "│"
        , " "
        ]

    -- How many nanoseconds each block represents?
    -- blocks take:
    -- width minus prefix size
    --       minus 4 for remainders of each phase
    blockMeasure :: Word64
    blockMeasure = longestPhase `div` fromIntegral (width - prefixSize - 4)

    formatSinglePhase :: Text -> Word64 -> Text
    formatSinglePhase colour phase
        | phase == 0 = ""
        | otherwise  = fmt [colour] $ stimes blockCount block
      where
        blockCount :: Word64
        blockCount = blockRemainder + div phase blockMeasure

        blockRemainder :: Word64
        blockRemainder = if phase `mod` blockMeasure > 0 then 1 else 0

fmt :: [Text] -> Text -> Text
fmt = formatWith


fmtNanos :: Word64 -> Text
fmtNanos time
    | time < ns  = "0ns"
    | time < mcs = show nanos   <> "ns"
    | time < ms  = show micros  <> "mcs"
    | time < s   = show millis  <> "ms"
    | time < m   = show seconds <> "s" <> emptyIfZero millis "ms"
    | otherwise  = show minutes <> "m" <> emptyIfZero seconds "s"
  where
    ns, mcs, ms, s, m :: Word64
    ns  = 1
    mcs = 1000 * ns
    ms  = 1000 * mcs
    s   = 1000 * ms
    m   = 60 * s

    nanos :: Word64
    nanos   = time `mod` mcs
    micros  = (time `div` mcs) `mod` 1000
    millis  = (time `div` ms)  `mod` 1000
    seconds = (time `div` s)   `mod` 60
    minutes = time `div` m

    emptyIfZero :: Word64 -> Text -> Text
    emptyIfZero 0 _    = ""
    emptyIfZero t unit = show t <> unit
