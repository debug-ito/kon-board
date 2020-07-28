{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
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

import Control.Applicative ((<|>), (<$>), empty)
import Control.Monad (forM, guard)
import Control.Monad.Logger (MonadLogger, logInfoN)
import Control.Monad.Trans (MonadIO(..))
import Data.Aeson (FromJSON(..), ToJSON(..))
import qualified Data.Aeson as Aeson
import qualified Data.Foldable as F
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text, pack)
import Data.Time (Day, fromGregorian)
import Data.Traversable (Traversable(..))
import GHC.Generics (Generic)

import KonBoard.MealPlan (MealPlan(..), MealPhase(..), toMealPhase, fromMealPhase)
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
openYAMLs :: (MonadLogger m, MonadIO m) => RecipeStore -> [FilePath] -> m YAMLStore
openYAMLs rstore files = do
  yaml_docs <- fmap concat $ forM files $ \file -> do
    logInfoN ("Read meal plan YAML file: " <> pack file)
    liftIO $ readYAMLDocs file
  liftIO $ fmap (YAMLStore . concat) $ traverse (fromMonthPlan rstore) $ yaml_docs

aesonOpt :: Aeson.Options
aesonOpt = Aeson.defaultOptions { Aeson.fieldLabelModifier = lmod }
  where
    lmod label = drop 3 label

fromMonthPlan :: RecipeStore -> MonthPlan -> IO [MealPlan]
fromMonthPlan rstore mp = traverse toMP $ mp_plan mp
  where
    toMP dp = do
      rsummaries <- traverse (loadRecipeByName' rstore) $ F.toList $ nonEmptyMeals $ dp_m dp
      return $ MealPlan
               { mealDay = fromGregorian (mp_year mp) (mp_month mp) (dp_d dp),
                 mealPhase = unYMealPhase $ dp_p dp,
                 mealRecipes = rsummaries,
                 mealNotes = [] -- TODO: load notes from the Store.
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
    dp_p :: YMealPhase,
    dp_m :: Meals
  }
  deriving (Show,Eq,Ord,Generic)

instance FromJSON DayPlan where
  parseJSON = Aeson.genericParseJSON aesonOpt

instance ToJSON DayPlan where
  toJSON = Aeson.genericToJSON aesonOpt
  toEncoding = Aeson.genericToEncoding aesonOpt

-- | MealPhase for YAML encoding.
newtype YMealPhase = YMealPhase { unYMealPhase :: MealPhase }
  deriving (Show,Eq,Ord)

instance FromJSON YMealPhase where
  parseJSON v = fmap YMealPhase $ parsedMealPhase
    where
      parsedMealPhase =
        case v of
          Aeson.String s -> either fail return $ toMealPhase s
          Aeson.Array a -> do
            guard (F.length a == 1)
            case F.toList a of
              [(Aeson.String p)] -> return $ MealOther p
              _ -> empty
          _ -> empty

instance ToJSON YMealPhase where
  toJSON (YMealPhase mp) =
    case mp of
      MealOther p -> toJSON [p]
      _ -> Aeson.String $ fromMealPhase mp

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
