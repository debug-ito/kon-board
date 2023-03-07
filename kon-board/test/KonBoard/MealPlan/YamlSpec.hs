module KonBoard.MealPlan.YamlSpec
    ( main
    , spec
    ) where

import           Control.Monad          (void)
import           Control.Monad.Logger   (LoggingT)
import           Control.Monad.Trans    (MonadIO (..))
import qualified Data.Text              as T
import           Data.Time              (fromGregorian)
import           Test.Hspec

import           KonBoard.MealPlan      (MealPhase (..), MealPlan (..))
import qualified KonBoard.MealPlan.Yaml as MY
import           KonBoard.Recipe        (Name, RecipeStore (..), RecipeStored)
import           KonBoard.Recipe.Memory (newRecipeStore)
import qualified KonBoard.Recipe.Yaml   as RY

import           KonBoard.TestLogger    (basicLogging)

main :: IO ()
main = hspec spec

runLog :: LoggingT IO a -> IO a
runLog = basicLogging

spec :: Spec
spec = do
  describe "readYamlFile" $ do
    specify "plan_example.yaml" $ runLog $ do
      sRecipe <- newRecipeStore
      void $ RY.loadYamlFile sRecipe "test/recipes/recipe_example.yaml"
      r1 <- getRecipe sRecipe "Name of the meal"
      r2 <- getRecipe sRecipe "Name of the external recipe"
      let expected =
            [ MealPlan
              { day = fromGregorian 2019 3 1
              , phase = Lunch
              , recipes = [r1]
              , notes = []
              }
            , MealPlan
              { day = fromGregorian 2019 3 1
              , phase = Dinner
              , recipes = [r1, r2]
              , notes = []
              }
            , MealPlan
              { day = fromGregorian 2019 3 1
              , phase = MealOther "late night snack"
              , recipes = [r1]
              , notes = []
              }
            , MealPlan
              { day = fromGregorian 2019 3 2
              , phase = Lunch
              , recipes = []
              , notes = ["TODO: make this meal plan ASAP"]
              }
            , MealPlan
              { day = fromGregorian 2019 3 2
              , phase = Dinner
              , recipes = [r1]
              , notes = ["Substitute sausages for bacons.", "TODO: buy ingredients"]
              }
            , MealPlan
              { day = fromGregorian 2019 4 1
              , phase = Lunch
              , recipes = [r1]
              , notes = []
              }
            ]
      got <- MY.readYamlFile sRecipe "test/meal-plans/plan_example.yaml"
      liftIO $ got `shouldBe` expected


getRecipe :: MonadFail m => RecipeStore m -> Name -> m RecipeStored
getRecipe s n = maybe (fail ("Cannot find recipe '" <> T.unpack n <> "'")) return =<< getRecipeByName s n

