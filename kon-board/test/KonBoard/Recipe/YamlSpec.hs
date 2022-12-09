module KonBoard.Recipe.YamlSpec
    ( main
    , spec
    ) where

import           Data.String.Interpolate       (i)
import           Test.Hspec

import           KonBoard.Recipe               (IngDesc (..), Ingredient (..), Ref (..))
import           KonBoard.Recipe.Internal.Type (Recipe (..))
import           KonBoard.Recipe.Yaml          (readYamlFile)

import           KonBoard.TestLogger           (basicLogging)



main :: IO ()
main = hspec spec

spec :: Spec
spec = specRecipe

specRecipe :: Spec
specRecipe = describe "readYamlFile" $ do
  specRecipeYaml "recipe_url.yaml"
    [ Recipe
      { _name = "external recipe with URL"
      , _ingredients = []
      , _description = ""
      , _references = [RefUrl "https://example.com/recipe/1102203" Nothing]
      , _rawYaml = "name: external recipe with URL\nurl: https://example.com/recipe/1102203\n"
      }
    ]
  specRecipeYaml "recipe_in.yaml"
    [ Recipe
      { _name = "internal recipe with ingredient groups"
      , _ingredients = [ IngSingle $ Ingredient "tomato" "1"
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
      , _description = "1. Cut the vegetables.\n"
                       <> "2. Cut the pork.\n"
                       <> "3. Soak the pork in (G2).\n"
                       <> "4. Stir-fry all ingredients in a pan with oil.\n"
                       <> "5. Put (G1).\n"
      , _references = []
      , _rawYaml = [i|name: internal recipe with ingredient groups
ings:
  - tomato, 1
  - carrot, 1/2
  - pepper, 3
  - pork, 200g
  - g: G1
    ings:
      - soy source, 1/2 spoon
      - miso, 1 spoon
  - g: G2
    ings:
      - sake, 2 spoon
      - mirin, 2 spoon
      - soy source, 2 spoon
  - oil, 1 spoon
desc: |
  1. Cut the vegetables.
  2. Cut the pork.
  3. Soak the pork in (G2).
  4. Stir-fry all ingredients in a pan with oil.
  5. Put (G1).


|]
      }
    ]
  specRecipeYaml "recipe_in_url.yaml"
    [ Recipe
      { _name = "internal recipe with URL"
      , _ingredients = [IngSingle $ Ingredient "beef" "300g"]
      , _description = "Roast the beef.\n"
      , _references = [RefUrl "http://example.com/reference/recipe/100" Nothing]
      , _rawYaml = [i|name: internal recipe with URL
ings:
  - beef, 300g
desc: |
  Roast the beef.
url: http://example.com/reference/recipe/100
|]
      }
    ]
  specRecipeYaml "recipe_multi.yaml"
    [ Recipe
      { _name = "recipe 1"
      , _ingredients = []
      , _description = ""
      , _references = [RefUrl "http://example.com/1" Nothing]
      , _rawYaml = [i|name: recipe 1
url: http://example.com/1
|]
      }
    , Recipe
      { _name = "recipe 2"
      , _ingredients = []
      , _description = ""
      , _references = [RefUrl "http://example.com/2" Nothing]
      , _rawYaml = [i|name: recipe 2
url: http://example.com/2
|]
      }
    ]
  specRecipeYaml "recipe_ext.yaml"
    [ Recipe
      { _name = "external recipe without URL"
      , _ingredients = []
      , _description = ""
      , _references = [RefSource "The recipe book, p.11"]
      , _rawYaml = [i|name: external recipe without URL
source: The recipe book, p.11
|]
      }
    ]
  specRecipeYaml "recipe_ext_url.yaml"
    [ Recipe
      { _name = "external recipe with source and URL."
      , _ingredients = []
      , _description = ""
      , _references = [RefUrl "http://example.com/recipe/foobar" (Just "ext recipe, with URL")]
      , _rawYaml = [i|name: external recipe with source and URL.
source: ext recipe, with URL
url: http://example.com/recipe/foobar
|]
      }
    ]
  specRecipeYaml "recipe_example.yaml"
    [ Recipe
      { _name = "Name of the meal"
      , _ingredients = [ IngSingle $ Ingredient "onion" "1"
                       , IngSingle $ Ingredient "bacon" "100g"
                       , IngGroup "★"
                         [ Ingredient "soy source" "1 spoon"
                         , Ingredient "miso" "1 spoon"
                         , Ingredient "water" "100ml"
                         ]
                       ]
      , _description = "1. Cut the onion and the bacon.\n"
                      <> "2. Put the onion, bacon and ★ into a pan and boil.\n"
      , _references = [RefUrl "http://example.com/recipe/101" Nothing]
      , _rawYaml = [i|\#\# A recipe with ingredients and steps.

\#\#\# Recipe name must be unique.
name: Name of the meal

\#\#\# List of ingredients.
ings:
  \# An ingredient is a pair of food item and its quantity, separated
  \# by a comma (",").
  - onion, 1
  - bacon, 100g

  \# You can make a group of ingredients. "g:" is a symbol for the
  \# group you use in the description below.
  - g: ★
    ings:
      - soy source, 1 spoon
      - miso, 1 spoon
      - water, 100ml

\#\#\# Description (steps) of the recipe. It's a Markdown text.
desc: |
  1. Cut the onion and the bacon.
  2. Put the onion, bacon and ★ into a pan and boil.

\#\#\# URL related to the recipe. This field is optional.
url: http://example.com/recipe/101


\# You can put multiple recipes in one YAML file, separated by "---"
\# line.
|]
      }
    , Recipe
      { _name = "Name of the external recipe"
      , _ingredients = []
      , _description = ""
      , _references = [RefUrl "https://example.com/some-recipe-book/11" $ Just "p.11, Some recipe book"]
      , _rawYaml = [i|
\#\# External recipe. The recipe body is somewhere else.

\#\#\# Name is mandatory even for an external recipe.
name: Name of the external recipe

\#\#\# The source of the recipe, for example, the name of the recipe book
\#\#\# and page number in it.
source: p.11, Some recipe book

\#\#\# External URL for the recipe.
url: https://example.com/some-recipe-book/11

\#\#\# External recipe must have either "source" or "url" field.

|]
      }
    ]

specRecipeYaml :: FilePath -> [Recipe] -> Spec
specRecipeYaml yamlFile expected = do
  specify yamlFile $ do
    (basicLogging $ readYamlFile yamlPath) `shouldReturn` expected
  where
    yamlPath = "test/recipes/" <> yamlFile
