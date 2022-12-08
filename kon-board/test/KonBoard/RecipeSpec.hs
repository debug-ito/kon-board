module KonBoard.RecipeSpec
    ( main
    , spec
    ) where

import qualified Data.Text                     as T
import           Test.Hspec

import           KonBoard.Recipe               (IngDesc (..), Ingredient (..), RecipeStored (..),
                                                Ref (..), parseIngredient, readYamlFile)
import           KonBoard.Recipe.Internal.Type (Recipe (..))

import           KonBoard.TestLogger           (basicLogging)


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  specIngredient
  specRecipe

specIngredient :: Spec
specIngredient = do
  describe "parseIngredient" $ do
    let theSpec input expected = do
          specify (T.unpack input) $ do
            (either (const $ Left ()) Right $ parseIngredient input) `shouldBe` expected
    theSpec "foo, bar" (Right $ Ingredient "foo" "bar")
    theSpec "foo  ,  quux  " (Right $ Ingredient "foo" "quux")
    theSpec ",foo" (Left ())
    theSpec "hoge," (Left ())
    theSpec "foo" (Left ())
    theSpec "" (Left ())

specRecipe :: Spec
specRecipe = describe "readYamlFile" $ do
  specRecipeYaml "recipe_url.yaml"
    [ Recipe
      { _name = "external recipe with URL"
      , _ingredients = []
      , _description = ""
      , _references = [RefUrl "https://example.com/recipe/1102203" Nothing]
      , _rawYaml = "TODO"
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
      , _rawYaml = "TODO"
      }
    ]
  specRecipeYaml "recipe_in_url.yaml"
    [ Recipe
      { _name = "internal recipe with URL"
      , _ingredients = [IngSingle $ Ingredient "beef" "300g"]
      , _description = "Roast the beef.\n"
      , _references = [RefUrl "http://example.com/reference/recipe/100" Nothing]
      , _rawYaml = "TODO"
      }
    ]
  specRecipeYaml "recipe_multi.yaml"
    [ Recipe
      { _name = "recipe 1"
      , _ingredients = []
      , _description = ""
      , _references = [RefUrl "http://example.com/1" Nothing]
      , _rawYaml = "TODO"
      }
    , Recipe
      { _name = "recipe 2"
      , _ingredients = []
      , _description = ""
      , _references = [RefUrl "http://example.com/2" Nothing]
      , _rawYaml = "TODO"
      }
    ]
  specRecipeYaml "recipe_ext.yaml"
    [ Recipe
      { _name = "external recipe without URL"
      , _ingredients = []
      , _description = ""
      , _references = [RefSource "The recipe book, p.11"]
      , _rawYaml = "TODO"
      }
    ]
  specRecipeYaml "recipe_ext_url.yaml"
    [ Recipe
      { _name = "external recipe with source and URL."
      , _ingredients = []
      , _description = ""
      , _references = [RefUrl "http://example.com/recipe/foobar" (Just "ext recipe, with URL")]
      , _rawYaml = "TODO"
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
      , _rawYaml = "TODO"
      }
    , Recipe
      { _name = "Name of the external recipe"
      , _ingredients = []
      , _description = ""
      , _references = [RefUrl "https://example.com/some-recipe-book/11" $ Just "p.11, Some recipe book"]
      , _rawYaml = "TODO"
      }
    ]

specRecipeYaml :: FilePath -> [Recipe] -> Spec
specRecipeYaml yamlFile expected = do
  specify yamlFile $ do
    (basicLogging $ readYamlFile yamlPath) `shouldReturn` expected
  where
    yamlPath = "test/recipes/" <> yamlFile
