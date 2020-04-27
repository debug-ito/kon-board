-- |
-- Module: KonBoard.Recipe
-- Description: Recipe data model
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module KonBoard.Recipe
  ( Recipe(..),
    Name,
    RecipeBody(..),
    URL,
    RecipeIn(..),
    IngDesc(..),
    IngGroupSymbol,
    Ingredient(..),
    FoodItem,
    Quantity
  ) where

import Data.Text (Text)
import Data.Yaml (FromJSON(..))

-- | Human-friendly name for a recipe.
type Name = Text

-- | URL text.
type URL = Text

-- | Description of recipe.
type Desc = Text

-- | A recipe.
data Recipe =
  Recipe
  { recipeName :: Name,
    recipeBody :: RecipeBody
  }
  deriving (Show,Eq,Ord)

instance FromJSON Recipe where
  parseJSON = undefined -- TODO

-- | Main content of a recipe.
data RecipeBody =
  -- | Internal recipe.
  RecipeBodyIn RecipeIn
  -- | External recipe, pointing to the URL.
  | RecipeBodyURL URL
  deriving (Show,Eq,Ord)

-- | Body of an internal recipe.
data RecipeIn =
  RecipeIn
  { recipeIngs :: [IngDesc],
    -- ^ Ingredients
    recipeDesc :: Desc,
    -- ^ Description of how to cook the dish.
    recipeRefURL :: Maybe URL
    -- ^ Reference URL of the internal recipe.
  }
  deriving (Show,Eq,Ord)

-- | Human-recognizable symbol for a group of ingredients.
type IngGroupSymbol = Text

-- | Ingredient description.
data IngDesc =
  IngGroup IngGroupSymbol [Ingredient]
  -- ^ Group of ingredients
  | IngSingle Ingredient
  -- ^ Single ingredient
  deriving (Show,Eq,Ord)

-- | Human-readable name of food item.
type FoodItem = Text

-- | Human-readable quantity of food item.
type Quantity = Text

-- | Ingredient.
data Ingredient = Ingredient FoodItem Quantity
                deriving (Show,Eq,Ord)
