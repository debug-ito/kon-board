-- | Recipe data model
module KonBoard.Recipe
    ( -- * Recipe
      Recipe (..)
    , Name
    , Url
    , Desc
      -- * Ref
    , Ref (..)
      -- * Ingredient
    , IngDesc (..)
    , IngGroupSymbol
    , Ingredient (..)
    , parseIngredient
    , FoodItem
    , Quantity
      -- * RecipeStore
    , RecipeStore (..)
    , Id
    , RecipeStored (..)
    ) where

import qualified Data.Text     as T

import           KonBoard.Base (Monoid (..), Text, when)

-- | Human-friendly name for a recipe.
type Name = Text

type Url = Text

-- | Description of recipe.
type Desc = Text

data Recipe
  = Recipe
      { name        :: Name
      , ingredients :: [IngDesc]
      , description :: Desc
      , references  :: [Ref]
      }
  deriving (Eq, Ord, Show)

-- | External reference of a recipe
data Ref
  = RefSource Text
  -- ^ Human-readable source of a recipe.
  | RefUrl Url (Maybe Text)
  -- ^ URL and optional anchor text.
  deriving (Eq, Ord, Show)

-- | Human-recognizable symbol for a group of ingredients.
type IngGroupSymbol = Text

-- | Ingredient description.
data IngDesc
  = IngGroup IngGroupSymbol [Ingredient]
  -- ^ Group of ingredients
  | IngSingle Ingredient
  -- ^ Single ingredient
  deriving (Eq, Ord, Show)

-- | Human-readable name of food item.
type FoodItem = Text

-- | Human-readable quantity of food item.
type Quantity = Text

-- | Ingredient.
data Ingredient
  = Ingredient FoodItem Quantity
  deriving (Eq, Ord, Show)

-- | Parse comman-separated food and quantity into 'Ingredent'.
parseIngredient :: Text -> Either String Ingredient
parseIngredient s = do
  let (rawFood, commaQtty) = T.break (== ',') s
      food = T.strip rawFood
  when (food == "") $ do
    Left (T.unpack s <> ": Empty food item name.")
  when (commaQtty == "") $ do
    Left (T.unpack s <> ": No comma is found.")
  let rawQtty = T.drop 1 commaQtty
      qtty = T.strip rawQtty
  when (qtty == "") $ do
    Left (T.unpack s <> ": Empty quantity.")
  return $ Ingredient food qtty

-- | URL-fiendly ID for a recipe
type Id = Text

-- | A 'Recipe' stored in 'RecipeStore'.
data RecipeStored
  = RecipeStored
      { id     :: Id
      , recipe :: Recipe
      }
  deriving (Eq, Ord, Show)

-- | Storage interface of recipes.
data RecipeStore m
  = RecipeStore
      { insertRecipe    :: Recipe -> m Id
      , updateRecipe    :: RecipeStored -> m ()
      , getRecipeById   :: Id -> m (Maybe RecipeStored)
      , getRecipeByName :: Name -> m (Maybe RecipeStored)
      }

