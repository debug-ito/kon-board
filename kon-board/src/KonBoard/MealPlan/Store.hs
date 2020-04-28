{-# LANGUAGE DeriveGeneric #-}
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

import Data.Aeson (FromJSON(..), ToJSON(..))
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import Data.Time (Day, fromGregorian)
import GHC.Generics (Generic)

import KonBoard.MealPlan (MealPlan, MealPhase)
import KonBoard.Recipe.Store (RecipeStore)
import KonBoard.Recipe (Name)

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

aesonOpt :: Aeson.Options
aesonOpt = Aeson.defaultOptions { Aeson.fieldLabelModifier = lmod }
  where
    lmod label = drop 3 label

-- TODO

---- fromMonthPlan :: RecipeStore -> MonthPlan -> IO [MealPlan]
---- fromMonthPlan rstore mp = do
---- 
---- map toMP $ mp_plan mp
----   where
----     toMP dp =
----       MealPlan
----       { mealDay = fromGregorian (mp_year mp) (mp_month mp) (dp_d dp),
----         mealPhase = dp_p dp,
----         mealRecipe
----       }

data MonthPlan =
  MonthPlan
  { mp_year :: Integer,
    mp_month :: Int,
    mp_plan :: [DayPlan]
  }
  deriving (Show,Eq,Ord,Generic)

instance FromJSON MonthPlan where
  parseJSON = Aeson.genericParseJSON aesonOpt

instance ToJSON MonthPlan where
  toJSON = Aeson.genericToJSON aesonOpt
  toEncoding = Aeson.genericToEncoding aesonOpt

data DayPlan =
  DayPlan
  { dp_d :: Int,
    dp_p :: MealPhase,
    dp_m :: Name
  }
  deriving (Show,Eq,Ord,Generic)

instance FromJSON DayPlan where
  parseJSON = Aeson.genericParseJSON aesonOpt

instance ToJSON DayPlan where
  toJSON = Aeson.genericToJSON aesonOpt
  toEncoding = Aeson.genericToEncoding aesonOpt


