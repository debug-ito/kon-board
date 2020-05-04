{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}
-- |
-- Module: KonBoard.Bridge.Time
-- Description: Easy-to-encode version of time-related types
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module KonBoard.Bridge.Time
  ( BDay,
    toBDay,
    fromBDay
  ) where

import Data.Aeson (defaultOptions)
import Data.Text (Text, unpack, pack)
import Data.Time (Day, defaultTimeLocale, parseTimeM, formatTime)
import qualified Elm.Derive as Elm
import Servant (FromHttpApiData)

-- $setup
--
-- >>> import Data.Time (toGregorian, fromGregorian)

-- | Text-encoded version of Day.
newtype BDay = BDay Text
             deriving (Show,Eq,Ord,FromHttpApiData)

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
