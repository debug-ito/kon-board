module KonBoard.Recipe.TestStore
    ( makeRecipeStoreSpec
    ) where

import           Control.Monad        (void)
import           Control.Monad.Logger (LoggingT)
import           Control.Monad.Trans  (MonadIO (..))
import           Data.Foldable        (traverse_)
import qualified Data.Text            as T
import           GHC.Records          (HasField (..))
import           Test.Hspec

import           KonBoard.Recipe      (Id, Name, RecipeStore (..), RecipeStored (..))
import           KonBoard.Recipe.Yaml (loadYamlFile, parseRecipe)

import           KonBoard.TestLogger  (basicLogging)

loadAndCheckName :: (MonadIO m, MonadFail m) => RecipeStore m -> Name -> m ()
loadAndCheckName store inputName = do
  (Just rs) <- getRecipeByName store inputName
  liftIO $ (getField @"name" $ getField @"recipe" rs) `shouldBe` inputName
  let rid = getField @"id" rs
  (Just rsById) <- getRecipeById store rid
  liftIO $ rsById `shouldBe` rs
  liftIO $ putStrLn ("Recipe: '" <> T.unpack inputName <> "' -> ID: " <> T.unpack rid)

makeRecipeStoreSpec :: String -> LoggingT IO (RecipeStore (LoggingT IO)) -> Spec
makeRecipeStoreSpec storeName makeStore = describe storeName $ do
  let commonYamlFiles = [ "recipe_in.yaml"
                        , "recipe_in_url.yaml"
                        , "recipe_multi.yaml"
                        , "recipe_url.yaml"
                        ]
      initStore = do
        rs <- makeStore
        traverse_ (loadYamlFile rs) $ map ("test/recipes/" <>) commonYamlFiles
        return rs
  specify "getRecipeByName, getRecipeById" $ basicLogging $ do
    store <- initStore
    loadAndCheckName store "internal recipe with ingredient groups"
    loadAndCheckName store "external recipe with URL"
    loadAndCheckName store "recipe 2"
  specify "loadRecipeByName, not found" $ basicLogging $ do
    store <- initStore
    got <- getRecipeByName store "this should not exist"
    liftIO $ got `shouldBe` Nothing
  specify "updateRecipe" $ basicLogging $ do
    store <- initStore
    (Just old) <- getRecipeByName store "recipe 1"
    let oldId = getField @"id" old
        newRecipeYaml = "name: recipe 1\ndesc: hoge hoge\n"
        (Right newRecipe) = parseRecipe newRecipeYaml
        newStored = RecipeStored { id = oldId, recipe = newRecipe }
    void $ updateRecipe store newStored
    gotByName <- getRecipeByName store "recipe 1"
    liftIO $ gotByName `shouldBe` Just newStored
    gotById <- getRecipeById store oldId
    liftIO $ gotById `shouldBe` Just newStored


---- This "stable recipeID" test is only for recipeStoreMemory. It's not for recipeStoreId
--
--   specify "recipeID has to be stable regardless of store" $ basicLogging $ do
--     let input = [ ["recipe_multi.yaml"],
--                   ["recipe_in.yaml", "recipe_url.yaml", "recipe_in_url.yaml", "recipe_multi.yaml"],
--                   ["recipe_url.yaml", "recipe_in.yaml", "recipe_multi.yaml", "recipe_in_url.yaml"]
--                 ]
--     got <- traverse (idForStore "recipe 2") $ input
--     liftIO $ (length $ nub got) `shouldBe` 1


