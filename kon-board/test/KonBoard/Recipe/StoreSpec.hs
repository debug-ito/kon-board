module KonBoard.Recipe.StoreSpec (main,spec) where

import Data.Monoid ((<>))
import Test.Hspec

import KonBoard.Recipe
  ( Recipe(..),
    Name
  )
import KonBoard.Recipe.Store
  ( RecipeStore,
    openYAMLs,
    loadRecipeByName,
    RecipeSummary(..),
    loadRecipe
  )

main :: IO ()
main = hspec spec

openStore :: IO RecipeStore
openStore = openYAMLs $ map ("test/recipe/" <>) files
  where
    files = [ "recipe_in.yaml",
              "recipe_in_url.yaml",
              "recipe_multi.yaml",
              "recipe_url.yaml"
            ]

loadAndCheckName :: RecipeStore -> Name -> IO ()
loadAndCheckName store name = do
  store <- openStore
  (Just rsummary) <- loadRecipeByName store name
  rsName rsummary `shouldBe` name
  recipe <- loadRecipe store $ rsID rsummary
  recipeName recipe `shouldBe` name
  
  
spec :: Spec
spec = describe "RecipeStore" $ do
  specify "loadRecipeByName, loadRecipe" $ do
    store <- openStore
    loadAndCheckName store "internal recipe with ingredient groups"
    loadAndCheckName store "external recipe with URL"
    loadAndCheckName store "recipe 2"
  specify "loadRecipeByName, not found" $ do
    store <- openStore
    got <- loadRecipeByName store "this should not exist"
    got `shouldBe` Nothing
