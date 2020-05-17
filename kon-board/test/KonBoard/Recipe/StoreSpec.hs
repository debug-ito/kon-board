module KonBoard.Recipe.StoreSpec (main,spec) where

import Data.List (reverse, nub)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Text (unpack)
import Data.Traversable (traverse)
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
    loadRecipe,
    ID
  )

import KonBoard.TestLogger (basicLogging)

main :: IO ()
main = hspec spec

openStore :: IO RecipeStore
openStore = basicLogging $ openYAMLs $ map ("test/recipes/" <>) files
  where
    files = [ "recipe_in.yaml",
              "recipe_in_url.yaml",
              "recipe_multi.yaml",
              "recipe_url.yaml"
            ]

idForStore :: Name -> [FilePath] -> IO ID
idForStore name files = do
  store <- basicLogging $ openYAMLs $ map ("test/recipes/" <>) files
  fmap (rsID . fromJust) $ loadRecipeByName store name

loadAndCheckName :: RecipeStore -> Name -> IO ()
loadAndCheckName store name = do
  (Just rsummary) <- loadRecipeByName store name
  rsName rsummary `shouldBe` name
  let rid = rsID rsummary
  recipe <- loadRecipe store $ rid
  putStrLn ("Recipe: '" <> unpack name <> "' -> ID: " <> unpack rid)
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
  specify "recipeID has to be stable regardless of store" $ do
    let input = [ ["recipe_multi.yaml"],
                  ["recipe_in.yaml", "recipe_url.yaml", "recipe_in_url.yaml", "recipe_multi.yaml"],
                  ["recipe_url.yaml", "recipe_in.yaml", "recipe_multi.yaml", "recipe_in_url.yaml"]
                ]
    got <- traverse (idForStore "recipe 2") $ input
    (length $ nub got) `shouldBe` 1
