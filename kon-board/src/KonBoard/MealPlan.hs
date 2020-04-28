{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: KonBoard.MealPlan
-- Description: Meal plan associated with recipes
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module KonBoard.MealPlan
  ( MealPhase(..),
    MealPlan(..)
  ) where

import Control.Applicative (empty)
import Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.=))
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import Data.Time (Day)

import KonBoard.Recipe.Store (RecipeSummary)

-- | Time of a meal in a day.
data MealPhase =
    Breakfast
  | Lunch
  | Dinner
  | MealOther Text
  deriving (Show,Eq,Ord)

instance FromJSON MealPhase where
  parseJSON (Aeson.String s) =
    case s of
      "breakfast" -> return Breakfast
      "lunch" -> return Lunch
      "dinner" -> return Dinner
      _ -> empty
  parseJSON (Aeson.Object o) = MealOther <$> (o .: "meal_phase")
  parseJSON _ = empty

instance ToJSON MealPhase where
  toJSON mp =
    case mp of
      Breakfast -> Aeson.String "breakfast"
      Lunch -> Aeson.String "lunch"
      Dinner -> Aeson.String "dinner"
      MealOther p -> Aeson.object ["meal_phase" .= p]

-- | Plan of a meal.
data MealPlan =
  MealPlan
  { mealDay :: Day,
    mealPhase :: MealPhase,
    mealRecipe :: RecipeSummary
  }
  deriving (Show,Eq,Ord)
