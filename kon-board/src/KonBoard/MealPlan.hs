{-# LANGUAGE OverloadedStrings #-}
-- |  Meal plan associated with recipes
module KonBoard.MealPlan
    ( MealPlan (..)
    , MealPhase (..)
    , Note
    , toMealPhase
    , fromMealPhase
    ) where

import           Control.Applicative   (empty)
import           Data.Aeson            (FromJSON (..), ToJSON (..), (.:), (.=))
import qualified Data.Aeson            as Aeson
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Data.Time             (Day)

import           KonBoard.Recipe.Store (RecipeSummary)

-- $setup
--
-- >>> :set -XOverloadedStrings

-- | Time of a meal in a day.
data MealPhase
  = Breakfast
  | Lunch
  | Dinner
  | MealOther Text
  deriving (Eq, Ord, Show)

-- | Parse 'Text' into 'MealPhase'.
--
-- >>> toMealPhase "breakfast"
-- Right Breakfast
-- >>> toMealPhase "lunch"
-- Right Lunch
-- >>> toMealPhase "dinner"
-- Right Dinner
-- >>> toMealPhase "@other meal phase"
-- Right (MealOther "other meal phase")
toMealPhase :: Text -> Either String MealPhase
toMealPhase t =
  case t of
    "breakfast" -> Right Breakfast
    "lunch" -> Right Lunch
    "dinner" -> Right Dinner
    _ -> maybe (Left ("Unknown MealPhase string: " <> show t)) (Right . MealOther)
         $ T.stripPrefix "@" t

-- | Encode 'MealPhase' into 'Text'.
--
-- >>> fromMealPhase Breakfast
-- "breakfast"
-- >>> fromMealPhase Lunch
-- "lunch"
-- >>> fromMealPhase Dinner
-- "dinner"
-- >>> fromMealPhase (MealOther "foo bar")
-- "@foo bar"
fromMealPhase :: MealPhase -> Text
fromMealPhase mp =
  case mp of
    Breakfast   -> "breakfast"
    Lunch       -> "lunch"
    Dinner      -> "dinner"
    MealOther t -> "@" <> t

-- | A human-readable note on a meal plan.
type Note = Text

-- | Plan of a meal.
data MealPlan
  = MealPlan
      { mealDay     :: Day
      , mealPhase   :: MealPhase
      , mealRecipes :: [RecipeSummary]
      , mealNotes   :: [Note]
      }
  deriving (Eq, Ord, Show)
