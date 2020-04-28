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
import Data.List (sort)
import Data.Text (Text)
import Data.Time (Day, fromGregorian)
import Data.Traversable (Traversable(..))
import GHC.Generics (Generic)

import KonBoard.MealPlan (MealPlan(..), MealPhase)
import KonBoard.Recipe.Store (RecipeStore, loadRecipeByName')
import KonBoard.Recipe (Name)
import KonBoard.Util.YAML (readYAMLDocs)

-- | Common API of 'MealPlan' store.
class AMealPlanStore s where
  searchMealPlans :: s -- ^ 'MealPlan' store
                  -> Day -- ^ start date (inclusive)
                  -> Day -- ^ end date (exclusive)
                  -> IO [MealPlan]

-- | 'MealPlan' store based on YAML files.
newtype YAMLStore = YAMLStore [MealPlan]

instance AMealPlanStore YAMLStore where
  searchMealPlans (YAMLStore mps) start end = return $ sort $ filter isInside mps
    where
      isInside mp = start <= mp_day && mp_day < end
        where
          mp_day = mealDay mp

-- | Read YAML files to make a 'YAMLStore'.
openYAMLs :: RecipeStore -> [FilePath] -> IO YAMLStore
openYAMLs rstore files = (fmap (YAMLStore . concat) . traverse (fromMonthPlan rstore))
                         =<< (fmap concat $ traverse readYAMLDocs files)

aesonOpt :: Aeson.Options
aesonOpt = Aeson.defaultOptions { Aeson.fieldLabelModifier = lmod }
  where
    lmod label = drop 3 label

fromMonthPlan :: RecipeStore -> MonthPlan -> IO [MealPlan]
fromMonthPlan rstore mp = traverse toMP $ mp_plan mp
  where
    toMP dp = do
      rsummary <- loadRecipeByName' rstore $ dp_m dp
      return $ MealPlan
               { mealDay = fromGregorian (mp_year mp) (mp_month mp) (dp_d dp),
                 mealPhase = dp_p dp,
                 mealRecipe = rsummary
               }

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


