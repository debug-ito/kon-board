{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
-- | Easy-to-encode version of time-related types
module KonBoard.Bridge.Time
    ( BDay
    , toBDay
    , fromBDay
    ) where

import           Data.Aeson  (defaultOptions)
import           Data.Text   (Text, pack, unpack)
import           Data.Time   (Day, defaultTimeLocale, formatTime, parseTimeM)
import qualified Elm.Derive  as Elm
import           Servant.API (FromHttpApiData)

-- $setup
--
-- >>> import Data.Time (toGregorian, fromGregorian)

-- | Text-encoded version of Day.
newtype BDay
  = BDay Text
  deriving (Eq, FromHttpApiData, Ord, Show)

$(Elm.deriveBoth defaultOptions ''BDay)

format :: String
format = "%Y-%m-%d"

-- | Convert 'Day' to 'SDay'.
--
-- >>> toBDay $ fromGregorian 2019 10 8
-- BDay "2019-10-08"
toBDay :: Day -> BDay
toBDay = BDay . pack . formatTime defaultTimeLocale format

-- | Convert 'SDay' to 'Day'.
--
-- >>> fmap toGregorian $ fromBDay $ BDay "2019-04-20"
-- Right (2019,4,20)
fromBDay :: BDay -> Either String Day
fromBDay (BDay input) =
  maybe (Left ("Failed to parse: " <> show input)) Right
  $ parseTimeM True defaultTimeLocale format
  $ unpack input
