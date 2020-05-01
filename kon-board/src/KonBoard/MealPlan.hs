{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: KonBoard.MealPlan
-- Description: Meal plan associated with recipes
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module KonBoard.MealPlan
  ( MealPlan(..),
    MealPhase(..),
    toMealPhase,
    fromMealPhase
  ) where

import Control.Applicative (empty)
import Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.=))
import qualified Data.Aeson as Aeson
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (Day)

import KonBoard.Recipe.Store (RecipeSummary)

-- $setup
--
-- >>> :set -XOverloadedStrings

-- | Time of a meal in a day.
data MealPhase =
    Breakfast
  | Lunch
  | Dinner
  | MealOther Text
  deriving (Show,Eq,Ord)

-- | Parse 'Text' into 'MealPhase'.
--
-- >>> toMealPhase "breakfast"
-- Right Breakfast
-- >>> toMealPhase "lunch"
-- Right Lunch
-- >>> toMealPhase "dinner"
-- Right Dinner
-- >>> toMealPhase "other:foo bar"
-- Right (MealOther "foo bar")
toMealPhase :: Text -> Either String MealPhase
toMealPhase t =
  case t of
    "breakfast" -> Right Breakfast
    "lunch" -> Right Lunch
    "dinner" -> Right Dinner
    _ -> maybe (Left ("Unknown MealPhase string: " <> show t)) (Right . MealOther)
         $ T.stripPrefix "other:" t

-- | Encode 'MealPhase' into 'Text'.
fromMealPhase :: MealPhase -> Text
fromMealPhase mp =
  case mp of
    Breakfast -> "breakfast"
    Lunch -> "lunch"
    Dinner -> "dinner"
    MealOther t -> "other:" <> t

instance FromJSON MealPhase where
  parseJSON (Aeson.String s) = either fail return $ toMealPhase s
  parseJSON _ = empty

instance ToJSON MealPhase where
  toJSON mp = Aeson.String $ fromMealPhase mp

-- | Plan of a meal.
data MealPlan =
  MealPlan
  { mealDay :: Day,
    mealPhase :: MealPhase,
    mealRecipes :: NonEmpty RecipeSummary
  }
  deriving (Show,Eq,Ord)
