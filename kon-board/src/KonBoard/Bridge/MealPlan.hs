{-# LANGUAGE TemplateHaskell #-}
-- | Easy-to-encode MealPlan data type variant
module KonBoard.Bridge.MealPlan
    ( BMealPlan
    , toBMealPlan
    ) where

import           Data.Time              (toGregorian)
import           Elm.Derive             (deriveBoth)

import           KonBoard.Base          (HasField (..), Text)
import           KonBoard.Bridge.Recipe (BRecipeStored, toBRecipeStored)
import           KonBoard.MealPlan      (MealPlan (..), fromMealPhase, toMealPhase)
import           KonBoard.Util.Aeson    (dropLabelOptions)

-- | Easy-to-encode version of 'MealPlan'.
data BMealPlan
  = BMealPlan
      { year    :: Integer
      , month   :: Int
      , day     :: Int
      , phase   :: Text
      , recipes :: [BRecipeStored]
      , notes   :: [Text]
      }
  deriving (Eq, Ord, Show)

toBMealPlan :: MealPlan -> BMealPlan
toBMealPlan mp =
  BMealPlan
  { year = y
  , month = m
  , day = d
  , phase = fromMealPhase $ getField @"phase" mp
  , recipes = map toBRecipeStored $ getField @"recipes" mp
  , notes = getField @"notes" mp
  }
  where
    (y, m, d) = toGregorian $ getField @"day" mp

$(deriveBoth (dropLabelOptions 0) ''BMealPlan)
