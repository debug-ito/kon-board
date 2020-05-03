module KonBoard.MealPlan.StoreSpec (main,spec) where

import Data.Foldable (toList)
import Data.Time (fromGregorian, Day)
import Test.Hspec

import KonBoard.Recipe (Name)
import qualified KonBoard.Recipe.Store as RStore
import KonBoard.Recipe.Store (RecipeSummary(..))
import KonBoard.MealPlan (MealPlan(..), MealPhase(..))
import KonBoard.MealPlan.Store
  ( AMealPlanStore(..), openYAMLs
  )

import KonBoard.TestLogger (basicLogging)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  spec_YAMLStore

day :: Integer -> Int -> Int -> Day
day = fromGregorian

spec_YAMLStore :: Spec
spec_YAMLStore = before makeStore $ specForStore "YAMLStore"
  where
    makeStore = basicLogging $ do
      rs <- RStore.openYAMLs recipe_files
      openYAMLs rs plan_files
    recipe_files = map ("test/recipe/" <>)
                   [ "recipe_in.yaml",
                     "recipe_in_url.yaml",
                     "recipe_multi.yaml",
                     "recipe_url.yaml"
                   ]
    plan_files = map ("test/meal-plan/" <>)
                 [ "plan1.yaml",
                   "plan2.yaml",
                   "plan3.yaml",
                   "plan_multi.yaml"
                 ]

planWithoutID :: MealPlan -> (Day, MealPhase, [Name])
planWithoutID mp = ( mealDay mp,
                     mealPhase mp,
                     map rsName $ toList $ mealRecipes mp
                   )

specForStore :: AMealPlanStore s => String -> SpecWith s
specForStore store_name = describe store_name $ do
  describe "searchMealPlans" $ do
    specify "empty result" $ \s -> do
      searchMealPlans s (day 2020 3 1) (day 2020 3 20) `shouldReturn` []
    specify "exclusive range" $ \s -> do
      let expected =
            [ (day 2020 4 1, Lunch, ["recipe 1"]),
              (day 2020 4 2, Dinner, ["external recipe with URL"])
            ]
      got <- searchMealPlans s (day 2020 4 1) (day 2020 4 10)
      map planWithoutID got `shouldBe` expected
    specify "lunch and dinner" $ \s -> do
      let expected =
            [ (day 2020 4 2, Dinner, ["external recipe with URL"]),
              (day 2020 4 10, Lunch, ["recipe 1"]),
              (day 2020 04 10, Dinner, ["recipe 2"])
            ]
      got <- searchMealPlans s (day 2020 4 2) (day 2020 4 11)
      map planWithoutID got `shouldBe` expected
    specify "sorted by day and phase" $ \s -> do
      let expected =
            [ (day 2020 4 23, Lunch, ["recipe 2"]),
              (day 2020 4 23, Dinner, ["recipe 2"]),
              (day 2020 5 15, Breakfast, ["internal recipe with ingredient groups"]),
              (day 2020 5 15, Lunch, ["recipe 2"])
            ]
      got <- searchMealPlans s (day 2020 4 23) (day 2020 5 16)
      map planWithoutID got `shouldBe` expected
    specify "MealPlan with multiple recipes" $ \s -> do
      let expected =
            [ (day 2019 11 21, Dinner, ["recipe 1", "recipe 2"])
            ]
      got <- searchMealPlans s (day 2019 11 1) (day 2019 11 30)
      map planWithoutID got `shouldBe` expected

