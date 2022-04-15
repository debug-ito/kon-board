module KonBoard.Recipe.MemorySpec
    ( main
    , spec
    ) where

import qualified Data.ByteString        as BS
import           Data.Monoid            ((<>))
import qualified Data.Text              as T
import           GHC.Records            (HasField (..))
import           Test.Hspec

import           KonBoard.Recipe        (IngDesc (..), Ingredient (..), Recipe (..),
                                         RecipeStored (..), Ref (..))
import           KonBoard.Recipe.Memory (getAllRecipes, readYaml)

import           KonBoard.TestLogger    (basicLogging)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  specRecipe

loadRecipes :: String -> IO [Recipe]
loadRecipes filename = do
  doc <- BS.readFile ("test/recipes/" ++ filename)
  fmap (map (getField @"recipe") . getAllRecipes) $ basicLogging $ readYaml doc

specRecipe :: Spec
specRecipe = describe "Recipe" $ do
  specRecipeYaml "recipe_url.yaml"
    [ Recipe
      { name = "external recipe with URL"
      , ingredients = []
      , description = ""
      , references = [RefUrl "https://example.com/recipe/1102203" Nothing]
      }
    ]
  specRecipeYaml "recipe_in.yaml"
    [ Recipe
      { name = "internal recipe with ingredient groups"
      , ingredients = [ IngSingle $ Ingredient "tomato" "1"
                      , IngSingle $ Ingredient "carrot" "1/2"
                      , IngSingle $ Ingredient "pepper" "3"
                      , IngSingle $ Ingredient "pork" "200g"
                      , IngGroup "G1"
                        [ Ingredient "soy source" "1/2 spoon"
                        , Ingredient "miso" "1 spoon"
                        ]
                      , IngGroup "G2"
                        [ Ingredient "sake" "2 spoon"
                        , Ingredient "mirin" "2 spoon"
                        , Ingredient "soy source" "2 spoon"
                        ]
                      , IngSingle $ Ingredient "oil" "1 spoon"
                      ]
      , description = "1. Cut the vegetables.\n"
                      <> "2. Cut the pork.\n"
                      <> "3. Soak the pork in (G2).\n"
                      <> "4. Stir-fry all ingredients in a pan with oil.\n"
                      <> "5. Put (G1).\n"
      , references = []
      }
    ]
  specRecipeYaml "recipe_in_url.yaml"
    [ Recipe
      { name = "internal recipe with URL"
      , ingredients = [IngSingle $ Ingredient "beef" "300g"]
      , description = "Roast the beef.\n"
      , references = [RefUrl "http://example.com/reference/recipe/100" Nothing]
      }
    ]
  specRecipeYaml "recipe_multi.yaml"
    [ Recipe
      { name = "recipe 1"
      , ingredients = []
      , description = ""
      , references = [RefUrl "http://example.com/1" Nothing]
      }
    , Recipe
      { name = "recipe 2"
      , ingredients = []
      , description = ""
      , references = [RefUrl "http://example.com/2" Nothing]
      }
    ]
  specRecipeYaml "recipe_ext.yaml"
    [ Recipe
      { name = "external recipe without URL"
      , ingredients = []
      , description = ""
      , references = [RefSource "The recipe book, p.11"]
      }
    ]
  specRecipeYaml "recipe_ext_url.yaml"
    [ Recipe
      { name = "external recipe with source and URL."
      , ingredients = []
      , description = ""
      , references = [RefUrl "http://example.com/recipe/foobar" (Just "ext recipe, with URL")]
      }
    ]
  specRecipeYaml "recipe_example.yaml"
    [ Recipe
      { name = "Name of the meal"
      , ingredients = [ IngSingle $ Ingredient "onion" "1"
                      , IngSingle $ Ingredient "bacon" "100g"
                      , IngGroup "★"
                        [ Ingredient "soy source" "1 spoon"
                        , Ingredient "miso" "1 spoon"
                        , Ingredient "water" "100ml"
                        ]
                      ]
      , description = "1. Cut the onion and the bacon.\n"
                      <> "2. Put the onion, bacon and ★ into a pan and boil."
      , references = [RefUrl "http://example.com/recipe/101" Nothing]
      }
    , Recipe
      { name = "Name of the external recipe"
      , ingredients = []
      , description = ""
      , references = [RefUrl "https://example.com/some-recipe-book/11" $ Just "p.11, Some recipe book"]
      }
    ]

specRecipeYaml :: FilePath -> [Recipe] -> Spec
specRecipeYaml yamlFile expected = do
  specify yamlFile $ do
    loadRecipes yamlFile `shouldReturn` expected

------ TODO: enable the following tests.

--- import KonBoard.Recipe
---   ( Recipe(..),
---     Name
---   )
--- import KonBoard.Recipe.Store
---   ( RecipeStore,
---     openYAMLs,
---     loadRecipeByName,
---     RecipeSummary(..),
---     loadRecipe,
---     ID
---   )
---
--- import KonBoard.TestLogger (basicLogging)
---
--- main :: IO ()
--- main = hspec spec
---
--- openStore :: IO RecipeStore
--- openStore = basicLogging $ openYAMLs $ map ("test/recipes/" <>) files
---   where
---     files = [ "recipe_in.yaml",
---               "recipe_in_url.yaml",
---               "recipe_multi.yaml",
---               "recipe_url.yaml"
---             ]
---
--- idForStore :: Name -> [FilePath] -> IO ID
--- idForStore name files = do
---   store <- basicLogging $ openYAMLs $ map ("test/recipes/" <>) files
---   fmap (rsID . fromJust) $ loadRecipeByName store name
---
--- loadAndCheckName :: RecipeStore -> Name -> IO ()
--- loadAndCheckName store name = do
---   (Just rsummary) <- loadRecipeByName store name
---   rsName rsummary `shouldBe` name
---   let rid = rsID rsummary
---   recipe <- loadRecipe store $ rid
---   putStrLn ("Recipe: '" <> unpack name <> "' -> ID: " <> unpack rid)
---   recipeName recipe `shouldBe` name
---
---
--- spec :: Spec
--- spec = describe "RecipeStore" $ do
---   specify "loadRecipeByName, loadRecipe" $ do
---     store <- openStore
---     loadAndCheckName store "internal recipe with ingredient groups"
---     loadAndCheckName store "external recipe with URL"
---     loadAndCheckName store "recipe 2"
---   specify "loadRecipeByName, not found" $ do
---     store <- openStore
---     got <- loadRecipeByName store "this should not exist"
---     got `shouldBe` Nothing
---   specify "recipeID has to be stable regardless of store" $ do
---     let input = [ ["recipe_multi.yaml"],
---                   ["recipe_in.yaml", "recipe_url.yaml", "recipe_in_url.yaml", "recipe_multi.yaml"],
---                   ["recipe_url.yaml", "recipe_in.yaml", "recipe_multi.yaml", "recipe_in_url.yaml"]
---                 ]
---     got <- traverse (idForStore "recipe 2") $ input
---     (length $ nub got) `shouldBe` 1
