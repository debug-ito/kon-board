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

import Control.Applicative ((<|>), (<$>))
import Data.Aeson (FromJSON(..), ToJSON(..))
import qualified Data.Aeson as Aeson
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty)
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
      rsummaries <- traverse (loadRecipeByName' rstore) $ nonEmptyMeals $ dp_m dp
      return $ MealPlan
               { mealDay = fromGregorian (mp_year mp) (mp_month mp) (dp_d dp),
                 mealPhase = dp_p dp,
                 mealRecipes = rsummaries
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
    dp_m :: Meals
  }
  deriving (Show,Eq,Ord,Generic)

instance FromJSON DayPlan where
  parseJSON = Aeson.genericParseJSON aesonOpt

instance ToJSON DayPlan where
  toJSON = Aeson.genericToJSON aesonOpt
  toEncoding = Aeson.genericToEncoding aesonOpt

data Meals =
    MealsSingle Name
  | MealsMulti (NonEmpty Name)
  deriving (Show,Eq,Ord,Generic)

nonEmptyMeals :: Meals -> NonEmpty Name
nonEmptyMeals (MealsSingle n) = return n
nonEmptyMeals (MealsMulti ns) = ns

instance FromJSON Meals where
  parseJSON v = (MealsSingle <$> parseJSON v)
                <|> (MealsMulti <$> parseJSON v)

instance ToJSON Meals where
  toJSON (MealsSingle n) = toJSON n
  toJSON (MealsMulti ns) = toJSON ns
