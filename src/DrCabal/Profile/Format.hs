module DrCabal.Profile.Format
    ( fmt
    , fmtNanos
    , fmtDecimalPlaces
    )where

import Colourista.Pure (formatWith)
import Data.Text (pack)
import Numeric (showFFloat)

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

fmtDecimalPlaces :: Int -> Float -> Text
fmtDecimalPlaces dp f = pack $ showFFloat (Just dp) f ""
