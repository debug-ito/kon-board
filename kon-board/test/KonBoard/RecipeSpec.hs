module KonBoard.RecipeSpec (main,spec) where

import Data.Monoid ((<>))
import Data.Yaml (decodeFileThrow)
import Test.Hspec

import KonBoard.Recipe
  ( Recipe(..), RecipeBody(..), RecipeIn(..), IngDesc(..), Ingredient(..)
  )

main :: IO ()
main = hspec spec

loadRecipe :: String -> IO Recipe
loadRecipe filename = decodeFileThrow ("test/recipe/" ++ filename)

spec :: Spec
spec = describe "Recipe" $ do
  specify "load RecipeURL from YAML" $ do
    let expected =
          Recipe
          { recipeName = "external recipe with URL",
            recipeBody = RecipeBodyURL "https://example.com/recipe/1102203"
          }
    got <- loadRecipe "recipe_url.yaml"
    got `shouldBe` expected
  specify "load RecipeIn from YAML" $ do
    let expected =
          Recipe
          { recipeName = "internal recipe with ingredient groups",
            recipeBody = RecipeBodyIn exp_body
          }
        exp_body =
          RecipeIn
          { recipeIngs = exp_ings,
            recipeDesc = exp_desc,
            recipeRefURL = Nothing
          }
        exp_desc = 
          "1. Cut the vegetables.\n"
          <> "2. Cut the pork.\n"
          <> "3. Soak the pork in (**).\n"
          <> "4. Stir-fry all ingredients in a pan with oil.\n"
          <> "5. Put (*)."
        exp_ings =
          [ IngSingle $ Ingredient "tomato" "1",
            IngSingle $ Ingredient "carrot" "1/2",
            IngSingle $ Ingredient "pepper" "3",
            IngSingle $ Ingredient "pork" "200g",
            IngGroup "*"
            [ Ingredient "soy source" "1/2 spoon",
              Ingredient "miso" "1 spoon"
            ],
            IngGroup "**"
            [ Ingredient "sake" "2 spoon",
              Ingredient "mirin" "2 spoon",
              Ingredient "soy source" "2 spoon"
            ],
            IngSingle $ Ingredient "oil" "1 spoon"
          ]
    got <- loadRecipe "recipe_in.yaml"
    got `shouldBe` expected

