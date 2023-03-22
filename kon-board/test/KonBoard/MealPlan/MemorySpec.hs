module KonBoard.MealPlan.MemorySpec
    ( main
    , spec
    ) where

import           Test.Hspec

import           KonBoard.MealPlan.Memory    (newMealPlanStore)

import           KonBoard.MealPlan.TestStore (mealPlanStoreSpec)

main :: IO ()
main = hspec spec

spec :: Spec
spec = before (return newMealPlanStore) $ mealPlanStoreSpec "newMealPlanStore"
