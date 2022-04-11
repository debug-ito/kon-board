-- | Recipe data model
module KonBoard.Recipe
    ( -- * Types
      Recipe (..)
    , Name
    , RecipeBody (..)
    , URL
    , RecipeExt (..)
    , RecipeIn (..)
    , Desc
    , IngDesc (..)
    , IngGroupSymbol
    , Ingredient (..)
    , FoodItem
    , Quantity
      -- * Reader
    , loadYAML
    ) where

import           Control.Applicative (empty, (<$>), (<*>))
import           Control.Monad       (when)
import           Data.Aeson          (FromJSON (..), (.:), (.:?))
import qualified Data.Aeson          as Aeson
import qualified Data.Aeson.KeyMap   as KM
import           Data.ByteString     (ByteString)
import qualified Data.ByteString     as BS
import           Data.Monoid         (mconcat)
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Traversable    (Traversable (traverse))
import qualified Data.Yaml           as YAML

import           KonBoard.Util.YAML  (decodeYAMLDocs)

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
      , references  :: [RecipeRef]
      }
  deriving (Eq, Ord, Show)

-- | External reference of a recipe
data RecipeRef
  = RecipeRefSource Text
  -- ^ Human-readable source of a recipe.
  | RecipeRefUrl Url (Maybe Text)
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

instance FromJSON IngDesc where
  parseJSON (Aeson.Object o) =
    IngGroup <$> (o .: "g") <*> (o .: "ings")
  parseJSON v = IngSingle <$> parseJSON v

-- | Human-readable name of food item.
type FoodItem = Text

-- | Human-readable quantity of food item.
type Quantity = Text

-- | Ingredient.
data Ingredient
  = Ingredient FoodItem Quantity
  deriving (Eq, Ord, Show)

instance FromJSON Ingredient where
  parseJSON (Aeson.String s) = do
    let (food, comma_qtty) = T.break (== ',') s
    when (food == "") $ do
      fail "Empty food item name."
    when (comma_qtty == "") $ do
      fail "Empty quantity."
    let qtty = T.drop 1 comma_qtty
    return $ Ingredient (T.strip food) (T.strip qtty)
  parseJSON _ = empty

-- | Load 'Recipe's from YAML data, possibly encoded in \"multiple
-- document\" encoding of YAML.
loadYAML :: ByteString -> Either YAML.ParseException [Recipe]
loadYAML = decodeYAMLDocs
