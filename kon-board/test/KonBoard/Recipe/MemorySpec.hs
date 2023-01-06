module KonBoard.Recipe.MemorySpec
    ( main
    , spec
    ) where

import           Control.Monad.Trans       (MonadIO (..))
import           Data.Foldable             (traverse_)
import           Data.List                 (nub)
import           Data.Maybe                (fromJust)
import           GHC.Records               (HasField (..))
import           Test.Hspec

import           KonBoard.Recipe           (Id, Name, RecipeStore (..), RecipeStored (..))
import           KonBoard.Recipe.Memory    (recipeStoreMemory)
import           KonBoard.Recipe.Yaml      (loadYamlFile)

import           KonBoard.Recipe.TestStore (recipeStoreSpec)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "recipeStoreMemory" $ do
  before recipeStoreMemory $ recipeStoreSpec
  specify "recipeID has to be stable regardless of store" $ do
    let input = [ ["recipe_multi.yaml"],
                  ["recipe_in.yaml", "recipe_url.yaml", "recipe_in_url.yaml", "recipe_multi.yaml"],
                  ["recipe_url.yaml", "recipe_in.yaml", "recipe_multi.yaml", "recipe_in_url.yaml"]
                ]
    got <- traverse (idForStore "recipe 2") $ input
    (length $ nub got) `shouldBe` 1

openStore :: [FilePath] -> IO (RecipeStore IO)
openStore files = do
  rs <- recipeStoreMemory
  traverse_ (loadYamlFile rs) $ map ("test/recipes/" <>) files
  return rs

idForStore :: Name -> [FilePath] -> IO Id
idForStore n files = do
  store <- openStore $ files
  fmap (getField @"id" . fromJust) $ getRecipeByName store n

