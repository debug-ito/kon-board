module KonBoard.Recipe.TestStore
    ( makeRecipeStoreSpec
    ) where

import           Control.Monad                 (forM_, void)
import           Control.Monad.Logger          (LoggingT)
import           Control.Monad.Trans           (MonadIO (..))
import           Data.Foldable                 (traverse_)
import qualified Data.Text                     as T
import           GHC.Records                   (HasField (..))
import           Test.Hspec

import           KonBoard.Recipe               (Id, Name, RecipeStore (..), RecipeStored (..),
                                                Ref (..))
import           KonBoard.Recipe.Internal.Type (Recipe (..))
import           KonBoard.Recipe.Yaml          (loadYamlFile, parseRecipe)

import           KonBoard.TestLogger           (basicLogging)

loadAndCheckName :: (MonadIO m, MonadFail m) => RecipeStore m -> Name -> m ()
loadAndCheckName store inputName = do
  (Just rs) <- getRecipeByName store inputName
  liftIO $ (getField @"name" $ getField @"recipe" rs) `shouldBe` inputName
  let rid = getField @"id" rs
  (Just rsById) <- getRecipeById store rid
  liftIO $ rsById `shouldBe` rs
  -- liftIO $ putStrLn ("Recipe: '" <> T.unpack inputName <> "' -> ID: " <> T.unpack rid)


-- TODO: change this to SpecWith (RecipeStore ...)

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
  describe "getRecipeByName" $ do
    specify "it should return the correct recipe content" $ basicLogging $ do
      store <- initStore
      (Just got) <- getRecipeByName store "recipe 1"
      let expected = RecipeStored { id = getField @"id" got
                                  , recipe = Recipe { _name = "recipe 1"
                                                    , _ingredients = []
                                                    , _description = ""
                                                    , _references = [RefUrl "http://example.com/1" Nothing]
                                                    , _rawYaml = "name: recipe 1\nurl: http://example.com/1\n"
                                                    }
                                  }
      liftIO $ got `shouldBe` expected
    specify "it should return Nothing for non-existent name" $ basicLogging $ do
      store <- initStore
      got <- getRecipeByName store "this should not exist"
      liftIO $ got `shouldBe` Nothing
    describe "it should match the recipe returned by getRecipeById" $ do
      let recipeNames = [ "internal recipe with ingredient groups"
                        , "external recipe with URL"
                        , "recipe 2"
                        ]
      forM_ recipeNames $ \recipeName -> do
        specify (T.unpack recipeName) $ basicLogging $ do
          store <- initStore
          loadAndCheckName store recipeName
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
