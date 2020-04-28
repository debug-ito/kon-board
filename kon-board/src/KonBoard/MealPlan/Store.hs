-- |
-- Module: KonBoard.MealPlan.Store
-- Description: Storage service for MealPlans.
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module KonBoard.MealPlan.Store
  ( -- * Common API
    AMealPlanStore(..),
    -- * YAMLStore
    YAMLStore,
    openYAMLs
  ) where

import Data.Time (Day)

import KonBoard.MealPlan (MealPlan)
import KonBoard.Recipe.Store (RecipeStore)

-- | Common API of 'MealPlan' store.
class AMealPlanStore s where
  searchMealPlans :: s -- ^ 'MealPlan' store
                  -> Day -- ^ start date (inclusive)
                  -> Day -- ^ end date (exclusive)
                  -> IO [MealPlan]

-- | 'MealPlan' store based on YAML files.
data YAMLStore = YAMLStore -- TODO

instance AMealPlanStore YAMLStore where
  searchMealPlans = undefined -- TODO

-- | Read YAML files to make a 'YAMLStore'.
openYAMLs :: RecipeStore -> [FilePath] -> IO YAMLStore
openYAMLs = undefined -- TODO
