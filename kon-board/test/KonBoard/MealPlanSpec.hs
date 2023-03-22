module KonBoard.MealPlanSpec
    ( main
    , spec
    ) where

import           Control.Monad     (forM_)
import           Test.Hspec

import           KonBoard.MealPlan (MealPhase (..), fromMealPhase, toMealPhase)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "MealPhase" $ do
    describe "toMealPhase / fromMealPhase" $ do
      let testSet = [ (Breakfast, "breakfast")
                    , (Lunch, "lunch")
                    , (Dinner, "dinner")
                    , (MealOther "other meal phase", "@other meal phase")
                    ]
      forM_ testSet $ \(mp, str) -> specify (show mp) $ do
        fromMealPhase mp `shouldBe` str
        toMealPhase str `shouldBe` Right mp
