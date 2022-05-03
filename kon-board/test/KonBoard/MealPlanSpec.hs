module KonBoard.MealPlanSpec
    ( main
    , spec
    ) where

import           Control.Monad     (forM_)
import           Data.Time         (Day, fromGregorian)
import           GHC.Records       (HasField (..))
import           Test.Hspec

import           KonBoard.MealPlan (MealPhase (..), MealPlan (..), MealPlanStore (..), Note,
                                    fromMealPhase, toMealPhase)
import           KonBoard.Recipe   (Name, Recipe (..), RecipeStored (..))

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "MealPhase" $ do
    specify "toMealPhase / fromMealPhase" $ do
      let testSet = [ (Breakfast, "breakfast")
                    , (Lunch, "lunch")
                    , (Dinner, "dinner")
                    , (MealOther "other meal phase", "@other meal phase")
                    ]
      forM_ testSet $ \(mp, str) -> do
        fromMealPhase mp `shouldBe` str
        toMealPhase str `shouldBe` Right mp

gday :: Integer -> Int -> Int -> Day
gday = fromGregorian

planWithoutID :: MealPlan -> (Day, MealPhase, [Name], [Note])
planWithoutID mp = ( getField @"day" mp
                   , getField @"phase" mp
                   , map (getField @"name" . getField @"recipe") $ getField @"recipes" mp
                   , getField @"notes" mp
                   )

specForStore :: String -> SpecWith (MealPlanStore m)
specForStore storeName = describe storeName $ do
  describe "searchMealPlans" $ do
    specify "empty result" $ \s -> do
      searchMealPlans s (gday 2020 3 1) (gday 2020 3 20) `shouldReturn` []
    specify "exclusive range" $ \s -> do
      let expected =
            [ (gday 2020 4 1, Lunch, ["recipe 1"], []),
              (gday 2020 4 2, Dinner, ["external recipe with URL"], [])
            ]
      got <- searchMealPlans s (gday 2020 4 1) (gday 2020 4 10)
      map planWithoutID got `shouldBe` expected
    specify "lunch and dinner" $ \s -> do
      let expected =
            [ (gday 2020 4 2, Dinner, ["external recipe with URL"], []),
              (gday 2020 4 10, Lunch, ["recipe 1"], []),
              (gday 2020 04 10, Dinner, ["recipe 2"], [])
            ]
      got <- searchMealPlans s (gday 2020 4 2) (gday 2020 4 11)
      map planWithoutID got `shouldBe` expected
    specify "sorted by gday and phase" $ \s -> do
      let expected =
            [ (gday 2020 4 23, Lunch, ["recipe 2"], []),
              (gday 2020 4 23, Dinner, ["recipe 2"], []),
              (gday 2020 5 15, Breakfast, ["internal recipe with ingredient groups"], []),
              (gday 2020 5 15, Lunch, ["recipe 2"], [])
            ]
      got <- searchMealPlans s (gday 2020 4 23) (gday 2020 5 16)
      map planWithoutID got `shouldBe` expected
    specify "MealPlan with multiple recipes" $ \s -> do
      let expected =
            [ (gday 2019 11 21, Dinner, ["recipe 1", "recipe 2"], [])
            ]
      got <- searchMealPlans s (gday 2019 11 1) (gday 2019 11 30)
      map planWithoutID got `shouldBe` expected
    specify "MealPlan in the example" $ \s -> do
      let expected =
            [ (gday 2019 3 1, Lunch, ["Name of the meal"], []),
              (gday 2019 3 1, Dinner, ["Name of the meal", "Name of the external recipe"], []),
              (gday 2019 3 1, MealOther "late night snack", ["Name of the meal"], []),
              (gday 2019 3 2, Lunch, [], ["TODO: make this meal plan ASAP"]),
              (gday 2019 3 2, Dinner, ["Name of the meal"], ["Substitute sausages for bacons.", "TODO: buy ingredients"]),
              (gday 2019 4 1, Lunch, ["Name of the meal"], [])
            ]
      got <- searchMealPlans s (gday 2019 3 1) (gday 2019 5 1)
      map planWithoutID got `shouldBe` expected
    specify "MealPlan with MealOther phases" $ \s -> do
      let expected =
            [ (exp_date, Breakfast, ["recipe 1"], []),
              (exp_date, Lunch, ["internal recipe with ingredient groups"], []),
              (exp_date, Dinner, ["Name of the meal"], []),
              (exp_date, MealOther "phase with at", ["recipe 2"], []),
              (exp_date, MealOther "phase with bracket", ["external recipe with URL"], [])
            ]
          exp_date = gday 2020 7 10
      got <- searchMealPlans s (gday 2020 7 10) (gday 2020 7 11)
      map planWithoutID got `shouldBe` expected
    specify "MealPlan with notes and possible no recipes" $ \s -> do
      let expected =
            [ (exp_date, Lunch, ["recipe 1"], ["a note."]),
              (exp_date, Dinner, [], ["multiple notes", "without recipe"])
            ]
          exp_date = gday 2020 7 15
      got <- searchMealPlans s (gday 2020 7 15) (gday 2020 7 16)
      map planWithoutID got `shouldBe` expected

