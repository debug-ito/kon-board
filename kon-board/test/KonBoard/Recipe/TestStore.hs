module KonBoard.Recipe.TestStore
    ( recipeStoreSpec
    , getRecipesByQuerySpec
    , loadCommonRecipes
    ) where

import           Control.Monad                 (forM_, void)
import           Control.Monad.Trans           (MonadIO (..))
import           Data.Foldable                 (traverse_)
import qualified Data.Text                     as T
import           GHC.Records                   (HasField (..))
import           Test.Hspec

import           KonBoard.Recipe               (Id, Name, RecipeStore (..), RecipeStored (..),
                                                Ref (..))
import           KonBoard.Recipe.Internal.Type (Recipe (..))
import           KonBoard.Recipe.Yaml          (loadYamlFile, parseRecipe)


loadAndCheckName :: RecipeStore IO -> Name -> IO ()
loadAndCheckName store inputName = do
  (Just rs) <- getRecipeByName store inputName
  (getField @"name" $ getField @"recipe" rs) `shouldBe` inputName
  let rid = getField @"id" rs
  (Just rsById) <- getRecipeById store rid
  rsById `shouldBe` rs
  -- putStrLn ("Recipe: '" <> T.unpack inputName <> "' -> ID: " <> T.unpack rid)

loadCommonRecipes :: RecipeStore IO -> IO (RecipeStore IO)
loadCommonRecipes rs = do
  traverse_ (loadYamlFile rs) $ map ("test/recipes/" <>) commonYamlFiles
  return rs
  where
    commonYamlFiles = [ "recipe_in.yaml"
                      , "recipe_in_url.yaml"
                      , "recipe_multi.yaml"
                      , "recipe_url.yaml"
                      ]

-- | Common spec of 'RecipeStore'. The caller must provide an empty 'RecipeStore'.
recipeStoreSpec :: SpecWith (RecipeStore IO)
recipeStoreSpec = beforeWith loadCommonRecipes $ specWithStore
  where
    specWithStore = do
      describe "getRecipeByName" $ do
        specify "it should return the correct recipe content" $ \store -> do
          (Just got) <- getRecipeByName store "recipe 1"
          let expected = RecipeStored { recipe = Recipe { _name = "recipe 1"
                                                        , _ingredients = []
                                                        , _description = ""
                                                        , _references = [RefUrl "http://example.com/1" Nothing]
                                                        , _rawYaml = "name: recipe 1\nurl: http://example.com/1\n"
                                                        }
                                      , id = getField @"id" got
                                      , createdAt = getField @"createdAt" got
                                      }
          got `shouldBe` expected
        specify "it should return Nothing for non-existent name" $ \store -> do
          got <- getRecipeByName store "this should not exist"
          got `shouldBe` Nothing
        describe "it should match the recipe returned by getRecipeById" $ do
          let recipeNames = [ "internal recipe with ingredient groups"
                            , "external recipe with URL"
                            , "recipe 2"
                            ]
          forM_ recipeNames $ \recipeName -> do
            specify (T.unpack recipeName) $ \store -> loadAndCheckName store recipeName
      specify "updateRecipe" $ \store -> do
        (Just old) <- getRecipeByName store "recipe 1"
        let oldId = getField @"id" old
            newRecipeYaml = "name: recipe 1\ndesc: hoge hoge\n"
            (Right newRecipe) = parseRecipe newRecipeYaml
            newStored = RecipeStored { recipe = newRecipe, id = oldId,  createdAt = getField @"createdAt" old}
        void $ updateRecipe store newStored
        gotByName <- getRecipeByName store "recipe 1"
        gotByName `shouldBe` Just newStored
        gotById <- getRecipeById store oldId
        gotById `shouldBe` Just newStored

getRecipesByQuerySpec :: SpecWith (RecipeStore IO)
getRecipesByQuerySpec = beforeWith loadCommonRecipes $ specWithStore
  where
    specWithStore = do
      describe "getRecipesByQuery" $ do
        specify "empty query should return all" $ \_ -> True `shouldBe` False -- TODO
        specify "count" $ \_ -> True `shouldBe` False -- TODO
        specify "offset" $ \_ -> True `shouldBe` False -- TODO
        specify "zero hit" $ \_ -> True `shouldBe` False -- TODO
        specify "hit by name" $ \_ -> True `shouldBe` False -- TODO
        specify "hit by ing" $ \_ -> True `shouldBe` False -- TODO
        specify "hit by desc" $ \_ -> True `shouldBe` False -- TODO
        specify "hit by ref source" $ \_ -> True `shouldBe` False -- TODO
        specify "hit by ref url" $ \_ -> True `shouldBe` False -- TODO
        specify "hit by name and desc" $ \_ -> True `shouldBe` False -- TODO
