module KonBoard.MealPlan.MemorySpec
    ( main
    , spec
    ) where

import           Test.Hspec

import           KonBoard.MealPlan.Memory (newMealPlanStore)

import           KonBoard.MealPlanSpec    (specForStore)

main :: IO ()
main = hspec spec

spec :: Spec
spec = before (return newMealPlanStore) $ specForStore "newMealPlanStore"
