-- |  Meal plan associated with recipes
module KonBoard.MealPlan
    ( MealPlan (..)
    , MealPhase (..)
    , Note
    , toMealPhase
    , fromMealPhase
    ) where

import qualified Data.Text       as T
import           Data.Time       (Day)

import           KonBoard.Base   (Text)
import           KonBoard.Recipe (RecipeStored)

-- | Time of a meal in a day.
data MealPhase
  = Breakfast
  | Lunch
  | Dinner
  | MealOther Text
  deriving (Eq, Ord, Show)

-- | Parse 'Text' into 'MealPhase'.
toMealPhase :: Text -> Either String MealPhase
toMealPhase t =
  case t of
    "breakfast" -> Right Breakfast
    "lunch" -> Right Lunch
    "dinner" -> Right Dinner
    _ -> maybe (Left ("Unknown MealPhase string: " <> show t)) (Right . MealOther)
         $ T.stripPrefix "@" t

-- | Encode 'MealPhase' into 'Text'.
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
      { day       :: Day
      , mealPhase :: MealPhase
      , recipes   :: [RecipeStored]
      , notes     :: [Note]
      }
  deriving (Eq, Ord, Show)
