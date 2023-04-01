module KonBoard.DbSpec
    ( main
    , spec
    ) where

import           Control.Exception.Safe      (throwString)
import           Control.Monad.Logger        (LoggingT)
import           GHC.Records                 (HasField (..))
import           System.Directory            (removeFile)
import           System.IO                   (hClose, openTempFile)
import           Test.Hspec

import           KonBoard.Db                 (Conn)
import qualified KonBoard.Db                 as Db
import           KonBoard.MealPlan           (MealPlanStore)
import           KonBoard.Recipe             (RecipeStore (..), RecipeStored (..), parseRecipe)

import           KonBoard.MealPlan.TestStore (mealPlanStoreSpec)
import           KonBoard.Recipe.TestStore   (getRecipesByQuerySpec, loadCommonRecipes,
                                              recipeStoreSpec)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  specRecipeStore
  specMealPlanStore

specRecipeStore :: Spec
specRecipeStore = do
  before openDbOnTempFile $ after closeDbOnTempFile $ do
    beforeWith getRecipeStore $ describe "recipeStoreDb" $ do
      recipeStoreSpec
      getRecipesByQuerySpec
      describe "updateRecipe" $ do
        specify "change name of a recipe" $ \store -> do
          _ <- loadCommonRecipes store
          (Just old) <- getRecipeByName store "recipe 1"
          newRecipe <- either throwString return $ parseRecipe "name: recipe 1 updated\nurl: http://example.com/2/new"
          let newInput = RecipeStored { recipe = newRecipe
                                      , id = getField @"id" old
                                      , createdAt = getField @"createdAt" old
                                      }
          updateRecipe store newInput
          gotForOldName <- getRecipeByName store "recipe 1"
          gotForOldName `shouldBe` Nothing
          gotForNewName <- getRecipeByName store "recipe 1 updated"
          gotForNewName `shouldBe` Just newInput
          gotForOldId <- getRecipeById store $ getField @"id" old
          gotForOldId `shouldBe` Just newInput

specMealPlanStore :: Spec
specMealPlanStore = do
  before openDbOnTempFile $ after closeDbOnTempFile $ do
    beforeWith getStores $ describe "mealPlanStoreDb" $ do
      mealPlanStoreSpec

openDbOnTempFile :: IO (Conn, FilePath)
openDbOnTempFile = do
  (path, h) <- openTempFile "test" "recipe.sqlite3"
  hClose h
  c <- Db.newSqliteConn path
  return (c, path)

closeDbOnTempFile :: (Conn, FilePath) -> IO ()
closeDbOnTempFile (c, path) = do
  Db.close c
  removeFile path

getRecipeStore :: (Conn, FilePath) -> IO (RecipeStore IO)
getRecipeStore (c, _) = return $ Db.recipeStoreDb c

getStores :: (Conn, FilePath) -> IO (RecipeStore (LoggingT IO), MealPlanStore (LoggingT IO))
getStores (c, _) = return (Db.recipeStoreDb c, Db.mealPlanStoreDb c)
