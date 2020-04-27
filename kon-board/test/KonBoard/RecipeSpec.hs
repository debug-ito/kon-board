module KonBoard.RecipeSpec (main,spec) where

import qualified Data.ByteString as BS
import Data.Monoid ((<>), mconcat)
import Test.Hspec

import KonBoard.Recipe
  ( Recipe(..), RecipeBody(..), RecipeIn(..), IngDesc(..), Ingredient(..),
    splitLineBS, loadYAML
  )

main :: IO ()
main = hspec spec

loadRecipes :: String -> IO [Recipe]
loadRecipes filename = do
  doc <- BS.readFile ("test/recipe/" ++ filename)
  either (fail . show) return $ loadYAML doc

spec :: Spec
spec = do
  spec_split
  spec_recipe

spec_split :: Spec
spec_split = describe "splitLineBS" $ do
  specify "empty" $ do
    splitLineBS "---" "" `shouldBe` [""]
  specify "line delimiter" $ do
    let input = mconcat $ map (<> "\n")
                [ "foo",
                  "---",
                  "bar",
                  "buzz",
                  "---",
                  "---",
                  "quux",
                  "hoge---",
                  ""
                ]
        expected = [ "foo\n",
                     "bar\nbuzz\n",
                     "",
                     "quux\nhoge---\n\n"
                   ]
    splitLineBS "---" input `shouldBe` expected
  specify "line delimiter at the head" $ do
    let input = mconcat $ map (<> "\n")
                [ "---",
                  "---",
                  "foo"
                ]
        expected = [ "",
                     "",
                     "foo\n"
                   ]
    splitLineBS "---" input `shouldBe` expected
  specify "line delimiter at the end" $ do
    let input = mconcat
                [ "foo\n",
                  "bar\n",
                  "---\n",
                  "buzz\n",
                  "---"
                ]
        expected = [ "foo\nbar\n",
                     "buzz\n",
                     ""
                   ]
    splitLineBS "---" input `shouldBe` expected

spec_recipe :: Spec
spec_recipe = describe "Recipe" $ do
  specify "load RecipeURL from YAML" $ do
    let expected =
          Recipe
          { recipeName = "external recipe with URL",
            recipeBody = RecipeBodyURL "https://example.com/recipe/1102203"
          }
    loadRecipes "recipe_url.yaml" `shouldReturn` [expected]
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
          <> "3. Soak the pork in (G2).\n"
          <> "4. Stir-fry all ingredients in a pan with oil.\n"
          <> "5. Put (G1).\n"
        exp_ings =
          [ IngSingle $ Ingredient "tomato" "1",
            IngSingle $ Ingredient "carrot" "1/2",
            IngSingle $ Ingredient "pepper" "3",
            IngSingle $ Ingredient "pork" "200g",
            IngGroup "G1"
            [ Ingredient "soy source" "1/2 spoon",
              Ingredient "miso" "1 spoon"
            ],
            IngGroup "G2"
            [ Ingredient "sake" "2 spoon",
              Ingredient "mirin" "2 spoon",
              Ingredient "soy source" "2 spoon"
            ],
            IngSingle $ Ingredient "oil" "1 spoon"
          ]
    loadRecipes "recipe_in.yaml" `shouldReturn` [expected]
  specify "load RecipeIn with reference URL from YAML" $ do
    let expected =
          Recipe
          { recipeName = "internal recipe with URL",
            recipeBody = RecipeBodyIn exp_body
          }
        exp_body =
          RecipeIn
          { recipeIngs = [IngSingle $ Ingredient "beef" "300g"],
            recipeDesc = "Roast the beef.\n",
            recipeRefURL = Just "http://example.com/reference/recipe/100"
          }
    loadRecipes "recipe_in_url.yaml" `shouldReturn` [expected]
  specify "load multiple recipes from YAML file" $ do
    let expected =
          [ Recipe
            { recipeName = "recipe 1",
              recipeBody = RecipeBodyURL "http://example.com/1"
            },
            Recipe
            { recipeName = "recipe 2",
              recipeBody = RecipeBodyURL "http://example.com/2"
            }
          ]
    loadRecipes "recipe_multi.yaml" `shouldReturn` expected

