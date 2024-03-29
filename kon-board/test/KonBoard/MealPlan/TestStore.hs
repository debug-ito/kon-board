module KonBoard.MealPlan.TestStore
    ( mealPlanStoreSpec
    ) where

import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Logger   (LoggingT)
import           Data.Time              (Day, fromGregorian)
import           GHC.Records            (HasField (..))
import           Test.Hspec

import           KonBoard.MealPlan      (MealPhase (..), MealPlan (..), MealPlanStore (..), Note,
                                         fromMealPhase, toMealPhase)
import qualified KonBoard.MealPlan.Yaml as MY
import           KonBoard.Recipe        (Name, Recipe, RecipeStore, RecipeStored (..))
import           KonBoard.Recipe.Memory (newRecipeStore)
import qualified KonBoard.Recipe.Yaml   as RY
import           KonBoard.TestLogger    (basicLogging)


gday :: Integer -> Int -> Int -> Day
gday = fromGregorian

planWithoutId :: MealPlan RecipeStored -> (Day, MealPhase, [Name], [Note])
planWithoutId mp = ( getField @"day" mp
                   , getField @"phase" mp
                   , map (getField @"name" . getField @"recipe") $ getField @"recipes" mp
                   , getField @"notes" mp
                   )

loadYamls :: (RecipeStore (LoggingT IO), MealPlanStore (LoggingT IO)) -> IO (MealPlanStore (LoggingT IO))
loadYamls (rs, ms) = basicLogging $ do
  mapM_ (RY.loadYamlFile rs) recipeFiles
  mapM_ (MY.loadYamlFile ms rs) planFiles
  return ms
  where
    recipeFiles = map ("test/recipes/" <>)
                  [ "recipe_in.yaml"
                  , "recipe_in_url.yaml"
                  , "recipe_multi.yaml"
                  , "recipe_url.yaml"
                  , "recipe_example.yaml"
                  ]
    planFiles = map ("test/meal-plans/" <>)
                [ "plan1.yaml"
                , "plan2.yaml"
                , "plan3.yaml"
                , "plan_multi.yaml"
                , "plan_example.yaml"
                , "plan_phases.yaml"
                , "plan_notes.yaml"
                ]

mealPlanStoreSpec :: SpecWith (RecipeStore (LoggingT IO), MealPlanStore (LoggingT IO))
mealPlanStoreSpec = beforeWith loadYamls $ do
  describe "getMealPlans" $ do
    specify "empty result" $ \s -> do
      got <- basicLogging $ getMealPlans s (gday 2020 3 1) (gday 2020 3 20)
      got `shouldBe` []
    specify "exclusive range" $ \s -> do
      let expected =
            [ (gday 2020 4 1, Lunch, ["recipe 1"], []),
              (gday 2020 4 2, Dinner, ["external recipe with URL"], [])
            ]
      got <- basicLogging $ getMealPlans s (gday 2020 4 1) (gday 2020 4 10)
      map planWithoutId got `shouldBe` expected
    specify "lunch and dinner" $ \s -> do
      let expected =
            [ (gday 2020 4 2, Dinner, ["external recipe with URL"], []),
              (gday 2020 4 10, Lunch, ["recipe 1"], []),
              (gday 2020 04 10, Dinner, ["recipe 2"], [])
            ]
      got <- basicLogging $ getMealPlans s (gday 2020 4 2) (gday 2020 4 11)
      map planWithoutId got `shouldBe` expected
    specify "sorted by gday and phase" $ \s -> do
      let expected =
            [ (gday 2020 4 23, Lunch, ["recipe 2"], []),
              (gday 2020 4 23, Dinner, ["recipe 2"], []),
              (gday 2020 5 15, Breakfast, ["internal recipe with ingredient groups"], []),
              (gday 2020 5 15, Lunch, ["recipe 2"], [])
            ]
      got <- basicLogging $ getMealPlans s (gday 2020 4 23) (gday 2020 5 16)
      map planWithoutId got `shouldBe` expected
    specify "MealPlan with multiple recipes" $ \s -> do
      let expected =
            [ (gday 2019 11 21, Dinner, ["recipe 1", "recipe 2"], [])
            ]
      got <- basicLogging $ getMealPlans s (gday 2019 11 1) (gday 2019 11 30)
      map planWithoutId got `shouldBe` expected
    specify "MealPlan in the example" $ \s -> do
      let expected =
            [ (gday 2019 3 1, Lunch, ["Name of the meal"], []),
              (gday 2019 3 1, Dinner, ["Name of the meal", "Name of the external recipe"], []),
              (gday 2019 3 1, MealOther "late night snack", ["Name of the meal"], []),
              (gday 2019 3 2, Lunch, [], ["TODO: make this meal plan ASAP"]),
              (gday 2019 3 2, Dinner, ["Name of the meal"], ["Substitute sausages for bacons.", "TODO: buy ingredients"]),
              (gday 2019 4 1, Lunch, ["Name of the meal"], [])
            ]
      got <- basicLogging $ getMealPlans s (gday 2019 3 1) (gday 2019 5 1)
      map planWithoutId got `shouldBe` expected
    specify "MealPlan with MealOther phases" $ \s -> do
      let expected =
            [ (exp_date, Breakfast, ["recipe 1"], []),
              (exp_date, Lunch, ["internal recipe with ingredient groups"], []),
              (exp_date, Dinner, ["Name of the meal"], []),
              (exp_date, MealOther "phase with at", ["recipe 2"], []),
              (exp_date, MealOther "phase with bracket", ["external recipe with URL"], [])
            ]
          exp_date = gday 2020 7 10
      got <- basicLogging $ getMealPlans s (gday 2020 7 10) (gday 2020 7 11)
      map planWithoutId got `shouldBe` expected
    specify "MealPlan with notes and possible no recipes" $ \s -> do
      let expected =
            [ (exp_date, Lunch, ["recipe 1"], ["a note."]),
              (exp_date, Dinner, [], ["multiple notes", "without recipe"])
            ]
          exp_date = gday 2020 7 15
      got <- basicLogging $ getMealPlans s (gday 2020 7 15) (gday 2020 7 16)
      map planWithoutId got `shouldBe` expected


