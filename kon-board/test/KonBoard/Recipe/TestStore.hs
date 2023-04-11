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

import           KonBoard.Query                (Answer (..), Query (..))
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

loadTestRecipes :: [FilePath] -> RecipeStore IO -> IO (RecipeStore IO)
loadTestRecipes files rs = do
  traverse_ (loadYamlFile rs) $ map ("test/recipes/" <>) files
  return rs

loadCommonRecipes :: RecipeStore IO -> IO (RecipeStore IO)
loadCommonRecipes = loadTestRecipes [ "recipe_in.yaml"
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
getRecipesByQuerySpec = beforeWith (loadTestRecipes ["recipe_query_test.yaml"]) $ specWithStore
  where
    specWithStore = do
      describe "getRecipesByQuery" $ do
        specify "empty query should return all" $ \_ -> True `shouldBe` False -- TODO
        specify "count" $ \_ -> True `shouldBe` False -- TODO
        specify "offset" $ \_ -> True `shouldBe` False -- TODO
        specify "zero hit" $ \_ -> True `shouldBe` False -- TODO
        specify "hit by name" $ \rs -> do
          got <- getRecipesByQuery rs $ qDef { query = "curry" }
          rNames got `shouldBe` (False, ["special curry rice"])
        specify "hit by ing" $ \rs -> do
          got <- getRecipesByQuery rs $ qDef { query = "onion" }
          rNames got `shouldBe` (False, ["with ings"])
        specify "hit by desc" $ \rs -> do
          got <- getRecipesByQuery rs $ qDef { query = "quux" }
          rNames got `shouldBe` (False, ["with desc"])
        specify "hit by ref source" $ \rs -> do
          got <- getRecipesByQuery rs $ qDef { query = "fuga" }
          rNames got `shouldBe` (False, ["with source"])
        specify "hit by ref url" $ \rs -> do
          got <- getRecipesByQuery rs $ qDef { query = "example.com" }
          rNames got `shouldBe` (False, ["special curry rice", "recipe 1"])
        specify "hit by name, ings and desc" $ \rs -> do
          got <- getRecipesByQuery rs $ qDef { query = "rice" }
          rNames got `shouldBe` (False, ["special curry rice", "R in ings", "R in desc"])
        specify "two query terms (AND condition)" $ \_ -> True `shouldBe` False -- TODO
    rNames ans = ( getField @"hasNext" ans
                 , map (getField @"name" . getField @"recipe") $ getField @"items" ans
                 )
    qDef = Query { query = "", count = 100, offset = 0 }
