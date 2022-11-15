{- |
Module                  : DrCabal.Model
Copyright               : (c) 2022 Dmitrii Kovanikov
SPDX-License-Identifier : MPL-2.0
Maintainer              : Dmitrii Kovanikov <kovanikov@gmail.com>
Stability               : Experimental
Portability             : Portable

Data types to model the domain of the @cabal@ output.
-}

module DrCabal.Model
    ( Style (..)
    , Line (..)
    , Status (..)
    , Entry (..)
    , parseLine
    ) where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, withText, (.:), (.=))

import qualified Data.Text as Text


data Style = Stacked

data Line = Line
    { lineTime :: Word64
    , lineLine :: ByteString
    } deriving stock (Show)

data Status
    = Downloading
    | Downloaded
    | Starting
    | Building
    | Haddock
    | Installing
    | Completed
    deriving stock (Show, Read, Eq, Ord, Enum, Bounded)

instance ToJSON Status where
    toJSON = toJSON @Text . show

instance FromJSON Status where
    parseJSON = withText "Status" $ \(toString -> t) -> case readMaybe t of
        Nothing     -> fail $ "Unexpected status: " <> t
        Just status -> pure status

data Entry = Entry
    { entryStatus  :: Status
    , entryStart   :: Word64
    , entryLibrary :: Text
    } deriving stock (Show)

instance ToJSON Entry where
    toJSON Entry{..} = object
        [ "status"    .= entryStatus
        , "startTime" .= entryStart
        , "library"   .= entryLibrary
        ]

instance FromJSON Entry where
    parseJSON = withObject "Entry" $ \o -> do
        entryStatus  <- o .: "status"
        entryStart   <- o .: "startTime"
        entryLibrary <- o .: "library"
        pure Entry{..}

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
