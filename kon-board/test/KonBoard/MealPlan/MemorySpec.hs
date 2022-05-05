module KonBoard.MealPlan.MemorySpec
    ( main
    , spec
    ) where

import           Test.Hspec

import           KonBoard.MealPlan.Memory (mealPlanStoreMemory)

import           KonBoard.MealPlanSpec    (specForStore)

main :: IO ()
main = hspec spec

spec :: Spec
spec = before mealPlanStoreMemory $ specForStore "mealPlanStoreMemory"
