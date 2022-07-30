{- |
Copyright: (c) 2022 Dmitrii Kovanikov
SPDX-License-Identifier: MPL-2.0
Maintainer: Dmitrii Kovanikov <kovanikov@gmail.com>

See README for more info
-}

module DrCabal.Profile
    ( profile
    ) where

import Colourista.Pure (blue, cyan, formatWith, red, yellow)
import Colourista.Short (b, u)
import Data.Aeson (eitherDecodeFileStrict')
import System.Console.ANSI (getTerminalSize)

import DrCabal.Cli (ProfileArgs (..))
import DrCabal.Model (Entry (..), Status (..))

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text


profile :: ProfileArgs -> IO ()
profile ProfileArgs{..} = do
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
        putStrLn $ "Error parsing file: " <> file
        putStrLn $ "    " <> err
        exitFailure
    Right entries -> pure entries

createProfileChart :: Int -> [Entry] -> Text
createProfileChart width l = case l of
    [] -> unlines
        [ "No cabal build entries found. Have you already built dependency?"
        , "Try removing global cabal store cache and rerunning again."
        ]
    entries ->
        let start = List.minimum $ map entryStart entries in
        formatChart width $ calculatePhases start $ groupEntries entries

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
    , phaseInstalling  :: Word64
    }

phaseTotal :: Phase -> Word64
phaseTotal (Phase p1 p2 p3 p4) = p1 + p2 + p3 + p4

calculatePhases :: Word64 -> Map Text [(Status, Word64)] -> Map Text Phase
calculatePhases start = fmap (entriesToPhase start)

entriesToPhase :: Word64 -> [(Status, Word64)] -> Phase
entriesToPhase start times = Phase
    { phaseDownloading = calcDownloading
    , phaseStarting    = calcStarting
    , phaseBuilding    = calcBuilding
    , phaseInstalling  = calcInstalling
    }
  where
    downloading, downloaded, starting, building, installing, completed :: Maybe Word64
    downloading = List.lookup Downloading times
    downloaded  = List.lookup Downloaded  times
    starting    = List.lookup Starting    times
    building    = List.lookup Building    times
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
    calcBuilding = case installing of
        Nothing -> 0
        Just it -> case building of
            Nothing -> it `minusw` start
            Just bt -> it `minusw` bt

    calcInstalling :: Word64
    calcInstalling = case completed of
        Nothing -> 0
        Just ct -> case installing of
            Nothing -> ct `minusw` start
            Just it -> ct `minusw` it

formatChart :: Int -> Map Text Phase -> Text
formatChart width libs = unlines $ legend ++ profile
  where
    block :: Text
    block = "▇"

    legend :: [Text]
    legend =
        [ ""
        , b "Legend"
        , "  " <> fmt [cyan]   block <> "  Downloading"
        , "  " <> fmt [blue]   block <> "  Starting"
        , "  " <> fmt [red]    block <> "  Building"
        , "  " <> fmt [yellow] block <> "  Installing"
        , ""
        ]

    profile :: [Text]
    profile =
        [ b " Dependency compilation profile result:"
        ] ++
        formattedEntries

    formattedEntries :: [Text]
    formattedEntries
        = map (uncurry formatRow)
        $ sortOn (Down . phaseTotal . snd) entries

    formatRow :: Text -> Phase -> Text
    formatRow libName Phase{..} = mconcat
        [ fmtLib libName
        , " "
        , "│"
        , " "
        , formatPhase cyan   phaseDownloading
        , formatPhase blue   phaseStarting
        , formatPhase red    phaseBuilding
        , formatPhase yellow phaseInstalling
        ]

    entries :: [(Text, Phase)]
    entries = Map.toList libs

    libSize :: Int
    libSize = List.maximum $ map (Text.length . fst) entries

    longestPhase :: Word64
    longestPhase = List.maximum $ map (phaseTotal . snd) entries

    fmtLib :: Text -> Text
    fmtLib = Text.justifyRight libSize ' '

    -- How many nanoseconds each block represents?
    -- blocks take:
    -- width minus lib-lenth
    --       minus 3 extra separators
    --       minus 2 right padding
    --       minus 4 for remainders of each phase
    blockMeasure :: Int
    blockMeasure = fromIntegral longestPhase `div` (width - libSize - 9)

    formatPhase :: Text -> Word64 -> Text
    formatPhase colour (fromIntegral -> phase)
        | phase == 0 = ""
        | otherwise  = fmt [colour] $ stimes blockCount block
      where
        blockCount :: Int
        blockCount = blockRemainder + div phase blockMeasure

        blockRemainder :: Int
        blockRemainder = if phase `mod` blockMeasure > 0 then 1 else 0

fmt :: [Text] -> Text -> Text
fmt = formatWith
