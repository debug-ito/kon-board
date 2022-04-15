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

parseIngredient :: Text -> Either String Ingredient
parseIngredient s = do
  let (food, comma_qtty) = T.break (== ',') s
  when (food == "") $ do
    Left "Empty food item name."
  when (comma_qtty == "") $ do
    Left "Empty quantity."
  let qtty = T.drop 1 comma_qtty
  return $ Ingredient (T.strip food) (T.strip qtty)

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
      { putRecipe       :: Recipe -> m Id
      , getRecipeById   :: Id -> m (Maybe RecipeStored)
      , getRecipeByName :: Name -> m (Maybe RecipeStored)
      }
