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

import Data.Text (Text)
import Data.Time (Day)

import KonBoard.Recipe.Store (RecipeSummary)

-- | Meal in a day.
data MealPhase =
  Breakfast
  | Lunch
  | Dinner
  | MealOther Text
  deriving (Show,Eq,Ord)

data MealPlan =
  MealPlan
  { mealDay :: Day,
    mealPhase :: MealPhase,
    mealRecipe :: RecipeSummary
  }
  deriving (Show,Eq,Ord)
