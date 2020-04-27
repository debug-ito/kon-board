-- |
-- Module: KonBoard.Recipe
-- Description: Recipe data model
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module KonBoard.Recipe
  ( Recipe(..),
    ID,
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

-- | URL-friendly ID for a recipe.
type ID = Text

-- | Human-friendly name for a recipe.
type Name = Text

-- | URL text.
type URL = Text

-- | Description of recipe.
type Desc = Text

-- | A recipe.
data Recipe =
  Recipe
  { recipeId :: ID,
    recipeName :: Name,
    recipeBody :: RecipeBody
  }
  deriving (Show,Eq,Ord)

-- | Main content of a recipe.
data RecipeBody =
  -- | Internal recipe.
  RecipeIn RecipeIn
  -- | External recipe, pointing to the URL.
  | RecipeURL URL
  deriving (Show,Eq,Ord)

-- | Body of an internal recipe.
data RecipeIn =
  RecipeIn
  { recipeIngs :: [IngDesc],
    -- ^ Ingredients
    recipeDesc :: Text
    -- ^ Description of how to cook the dish.
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
