module KonBoard.MealPlan.MemorySpec
    ( main
    , spec
    ) where

import           Control.Monad.Logger        (LoggingT)
import           Test.Hspec

import           KonBoard.MealPlan           (MealPlanStore)
import           KonBoard.MealPlan.Memory    (newMealPlanStore)
import           KonBoard.Recipe             (RecipeStore)
import           KonBoard.Recipe.Memory      (newRecipeStore)

import           KonBoard.MealPlan.TestStore (mealPlanStoreSpec)

main :: IO ()
main = hspec spec

initStores :: IO (RecipeStore (LoggingT IO), MealPlanStore (LoggingT IO))
initStores = do
  rs <- newRecipeStore
  ms <- newMealPlanStore rs
  return (rs, ms)

spec :: Spec
spec = before initStores $ mealPlanStoreSpec
