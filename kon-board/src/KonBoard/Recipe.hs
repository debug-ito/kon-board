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

import Control.Monad (when)
import Control.Applicative ((<$>), (<*>), empty)
import Data.Aeson (FromJSON(..), (.:), (.:?))
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HS
import Data.Text (Text)
import qualified Data.Text as T


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
  parseJSON v@(Aeson.Object o) =
    Recipe <$> (o .: "name") <*> parseJSON v
  parseJSON _ = empty

-- | Main content of a recipe.
data RecipeBody =
  -- | Internal recipe.
  RecipeBodyIn RecipeIn
  -- | External recipe, pointing to the URL.
  | RecipeBodyURL URL
  deriving (Show,Eq,Ord)

instance FromJSON RecipeBody where
  parseJSON v@(Aeson.Object o) = 
    if HS.member "desc" o
    then RecipeBodyIn <$> parseJSON v
    else RecipeBodyURL <$> (o .: "url")
  parseJSON _ = empty

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

instance FromJSON RecipeIn where
  parseJSON (Aeson.Object o) =
    RecipeIn <$> (o .: "ings") <*> (o .: "desc") <*> (o .:? "url")
  parseJSON _ = empty
    

-- | Human-recognizable symbol for a group of ingredients.
type IngGroupSymbol = Text

-- | Ingredient description.
data IngDesc =
  IngGroup IngGroupSymbol [Ingredient]
  -- ^ Group of ingredients
  | IngSingle Ingredient
  -- ^ Single ingredient
  deriving (Show,Eq,Ord)

instance FromJSON IngDesc where
  parseJSON (Aeson.Object o) =
    IngGroup <$> (o .: "g") <*> (o .: "i")
  parseJSON v = IngSingle <$> parseJSON v

-- | Human-readable name of food item.
type FoodItem = Text

-- | Human-readable quantity of food item.
type Quantity = Text

-- | Ingredient.
data Ingredient = Ingredient FoodItem Quantity
                deriving (Show,Eq,Ord)

instance FromJSON Ingredient where
  parseJSON (Aeson.String s) = do
    let (food, comma_qtty) = T.break (== ',') s
    when (food == "") $ do
      fail "Empty food item name."
    when (comma_qtty == "") $ do
      fail "Empty quantity."
    let qtty = T.drop 1 comma_qtty
    return $ Ingredient food qtty
  parseJSON _ = empty
