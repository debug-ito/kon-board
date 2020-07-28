module KonBoard.MealPlan.StoreSpec (main,spec) where

import Data.Foldable (toList)
import Data.Time (fromGregorian, Day)
import Test.Hspec

import KonBoard.Recipe (Name)
import qualified KonBoard.Recipe.Store as RStore
import KonBoard.Recipe.Store (RecipeSummary(..))
import KonBoard.MealPlan (MealPlan(..), MealPhase(..), Note)
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
    recipe_files = map ("test/recipes/" <>)
                   [ "recipe_in.yaml",
                     "recipe_in_url.yaml",
                     "recipe_multi.yaml",
                     "recipe_url.yaml",
                     "recipe_example.yaml"
                   ]
    plan_files = map ("test/meal-plans/" <>)
                 [ "plan1.yaml",
                   "plan2.yaml",
                   "plan3.yaml",
                   "plan_multi.yaml",
                   "plan_example.yaml",
                   "plan_phases.yaml",
                   "plan_notes.yaml"
                 ]

planWithoutID :: MealPlan -> (Day, MealPhase, [Name], [Note])
planWithoutID mp = ( mealDay mp,
                     mealPhase mp,
                     map rsName $ toList $ mealRecipes mp,
                     toList $ mealNotes mp
                   )

specForStore :: AMealPlanStore s => String -> SpecWith s
specForStore store_name = describe store_name $ do
  describe "searchMealPlans" $ do
    specify "empty result" $ \s -> do
      searchMealPlans s (day 2020 3 1) (day 2020 3 20) `shouldReturn` []
    specify "exclusive range" $ \s -> do
      let expected =
            [ (day 2020 4 1, Lunch, ["recipe 1"], []),
              (day 2020 4 2, Dinner, ["external recipe with URL"], [])
            ]
      got <- searchMealPlans s (day 2020 4 1) (day 2020 4 10)
      map planWithoutID got `shouldBe` expected
    specify "lunch and dinner" $ \s -> do
      let expected =
            [ (day 2020 4 2, Dinner, ["external recipe with URL"], []),
              (day 2020 4 10, Lunch, ["recipe 1"], []),
              (day 2020 04 10, Dinner, ["recipe 2"], [])
            ]
      got <- searchMealPlans s (day 2020 4 2) (day 2020 4 11)
      map planWithoutID got `shouldBe` expected
    specify "sorted by day and phase" $ \s -> do
      let expected =
            [ (day 2020 4 23, Lunch, ["recipe 2"], []),
              (day 2020 4 23, Dinner, ["recipe 2"], []),
              (day 2020 5 15, Breakfast, ["internal recipe with ingredient groups"], []),
              (day 2020 5 15, Lunch, ["recipe 2"], [])
            ]
      got <- searchMealPlans s (day 2020 4 23) (day 2020 5 16)
      map planWithoutID got `shouldBe` expected
    specify "MealPlan with multiple recipes" $ \s -> do
      let expected =
            [ (day 2019 11 21, Dinner, ["recipe 1", "recipe 2"], [])
            ]
      got <- searchMealPlans s (day 2019 11 1) (day 2019 11 30)
      map planWithoutID got `shouldBe` expected
    specify "MealPlan in the example" $ \s -> do
      let expected =
            [ (day 2019 3 1, Lunch, ["Name of the meal"], []),
              (day 2019 3 1, Dinner, ["Name of the meal", "Name of the external recipe"], []),
              (day 2019 3 1, MealOther "late night snack", ["Name of the meal"], []),
              (day 2019 4 1, Lunch, ["Name of the meal"], [])
            ]
      got <- searchMealPlans s (day 2019 3 1) (day 2019 5 1)
      map planWithoutID got `shouldBe` expected
    specify "MealPlan with MealOther phases" $ \s -> do
      let expected =
            [ (exp_date, Breakfast, ["recipe 1"], []),
              (exp_date, Lunch, ["internal recipe with ingredient groups"], []),
              (exp_date, Dinner, ["Name of the meal"], []),
              (exp_date, MealOther "phase with at", ["recipe 2"], []),
              (exp_date, MealOther "phase with bracket", ["external recipe with URL"], [])
            ]
          exp_date = day 2020 7 10
      got <- searchMealPlans s (day 2020 7 10) (day 2020 7 11)
      map planWithoutID got `shouldBe` expected
    specify "MealPlan with notes and possible no recipes" $ \s -> do
      let expected =
            [ (exp_date, Lunch, ["recipe 1"], ["a note."]),
              (exp_date, Dinner, [], ["multiple notes", "without recipe"])
            ]
          exp_date = day 2020 7 15
      got <- searchMealPlans s (day 2020 7 15) (day 2020 7 16)
      map planWithoutID got `shouldBe` expected

