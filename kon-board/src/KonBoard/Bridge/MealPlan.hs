{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module: KonBoard.Bridge.MealPlan
-- Description: Easy-to-encode MealPlan data type variant
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module KonBoard.Bridge.MealPlan
  ( BMealPlan(..)
  ) where

import Data.Text (Text)
import qualified Elm.Derive as Elm

import KonBoard.Bridge.Util (dropLabelOptions)
import KonBoard.Bridge.Recipe (BRecipeSummary)

data BMealPlan =
  BMealPlan
  { bm_year :: Integer,
    bm_month :: Int,
    bm_day :: Int,
    bm_phase :: Text,
    bm_recipe_summary :: BRecipeSummary
  }
  deriving (Show,Eq,Ord)

$(Elm.deriveBoth (dropLabelOptions 3) ''BMealPlan)
