{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module: KonBoard.Bridge.MealPlan
-- Description: Easy-to-encode MealPlan data type variant
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module KonBoard.Bridge.MealPlan
  ( BMealPlan,
    toBMealPlan,
    fromBMealPlan
  ) where

import Data.Foldable (toList)
import Data.Text (Text)
import Data.Time (toGregorian, fromGregorian)
import qualified Elm.Derive as Elm

import KonBoard.Bridge.Util (dropLabelOptions)
import KonBoard.Bridge.Recipe
  ( BRecipeSummary, toBRecipeSummary, fromBRecipeSummary
  )
import KonBoard.MealPlan (MealPlan(..), fromMealPhase, toMealPhase)

-- | Easy-to-encode version of 'MealPlan'.
data BMealPlan =
  BMealPlan
  { bm_year :: Integer,
    bm_month :: Int,
    bm_day :: Int,
    bm_phase :: Text,
    bm_recipes :: [BRecipeSummary],
    bm_notes :: [Text]
  }
  deriving (Show,Eq,Ord)

toBMealPlan :: MealPlan -> BMealPlan
toBMealPlan mp =
  BMealPlan
  { bm_year = year,
    bm_month = month,
    bm_day = day,
    bm_phase = fromMealPhase $ mealPhase mp,
    bm_recipes = map toBRecipeSummary $ mealRecipes mp,
    bm_notes = mealNotes mp
  }
  where
    (year, month, day) = toGregorian $ mealDay mp

fromBMealPlan :: BMealPlan -> Either String MealPlan
fromBMealPlan bm = do
  phase <- toMealPhase $ bm_phase bm
  return $ MealPlan { mealDay = fromGregorian (bm_year bm) (bm_month bm) (bm_day bm),
                      mealPhase = phase,
                      mealRecipes = fmap fromBRecipeSummary $ bm_recipes bm,
                      mealNotes = bm_notes bm
                    }
    
$(Elm.deriveBoth (dropLabelOptions 3) ''BMealPlan)
