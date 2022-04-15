{-# LANGUAGE TemplateHaskell #-}
-- | Easy-to-encode version of time-related types
module KonBoard.Bridge.Time
    ( BDay
    , toBDay
    , fromBDay
    ) where

import           Data.Aeson  (defaultOptions)
import           Data.Text   (Text, pack, unpack)
import           Data.Time   (Day, defaultTimeLocale, formatTime, parseTimeM)
import           Elm.Derive  (deriveBoth)
import           Servant.API (FromHttpApiData, ToHttpApiData)

-- | Text-encoded version of Day.
newtype BDay
  = BDay Text
  deriving (Eq, FromHttpApiData, Ord, Show, ToHttpApiData)

$(deriveBoth defaultOptions ''BDay)

format :: String
format = "%Y-%m-%d"

toBDay :: Day -> BDay
toBDay = BDay . pack . formatTime defaultTimeLocale format

fromBDay :: BDay -> Either String Day
fromBDay (BDay input) =
  maybe (Left ("Failed to parse: " <> show input)) Right
  $ parseTimeM True defaultTimeLocale format
  $ unpack input

