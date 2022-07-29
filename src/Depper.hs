{- |
Copyright: (c) 2022 Dmitrii Kovanikov
SPDX-License-Identifier: MPL-2.0
Maintainer: Dmitrii Kovanikov <kovanikov@gmail.com>

See README for more info
-}

module Depper
    ( main
    ) where

import System.IO (isEOF)
import Data.Time.Clock.POSIX (getPOSIXTime, POSIXTime)
import Text.Pretty.Simple (pPrint)

import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text


data Line = Line
    { lineTime :: POSIXTime
    , lineLine :: ByteString
    } deriving stock (Show)

main :: IO ()
main = go []
  where
    go :: [Line] -> IO ()
    go input = do
        isEndOfInput <- isEOF
        if isEndOfInput
        then do
            -- writeFile "depper.debug" $ show input
            let chart = createPerformanceChart input
            -- putTextLn chart
            pPrint chart
        else do
            time <- getPOSIXTime
            line <- ByteString.getLine
            BS8.putStrLn $ show time <> "  >>>  " <> line
            go $ Line time line : input

createPerformanceChart :: [Line] -> [Entry]
createPerformanceChart = mapMaybe parseLine . reverse

data Status
    = Downloading
    | Downloaded
    | Starting
    | Building
    | Installing
    | Completed
    deriving stock (Show, Read, Eq, Ord, Enum, Bounded)

data Entry = Entry
    { entryStatus :: Text
    , entryStart  :: POSIXTime
    , entryLibrary :: Text
    } deriving stock (Show)

parseLine :: Line -> Maybe Entry
parseLine Line{..} = do
    let txtLine = decodeUtf8 lineLine
    txtStatus : library : _ <- Just $ words txtLine

--    -- parse status string to the 'Status' type
--    status <- readMaybe $ toString txtStatus
--
--    -- check if this line is a library: '-' separates library name and its version
--    guard $ Text.elem '-' library
--
--    pure $ Entry
--        { entryStatus  = status
--        , entryStart   = lineTime
--        , entryLibrary = library
--        }
