-- | Recipe data model
module KonBoard.Recipe
    ( -- * Recipe
      Recipe
    , Name
    , Url
    , Desc
    , parseRecipe
      -- ** YAML readers
    , readYaml
    , readYamlFile
    , loadYamlFile
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

import           Data.Aeson          (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON)
import qualified Data.ByteString     as BS
import qualified Data.Text           as T

import           KonBoard.Base       (ByteString, Generic, HasField (..), MonadIO, MonadThrow, Text,
                                      liftIO, throw, throwString, traverse)
import           KonBoard.Util.Aeson (dropLabelOptions)
import           KonBoard.Util.Yaml  (decodeYAMLDocs)



import           KonBoard.Base       (Monoid (..), Text, when)

-- | Human-friendly name for a recipe.
type Name = Text

type Url = Text

-- | Description of recipe.
type Desc = Text

-- | An opaque type for a recipe. Use 'parseRecipe' to construct it.
data Recipe
  = Recipe
      { _name        :: Name
      , _ingredients :: [IngDesc]
      , _description :: Desc
      , _references  :: [Ref]
      , _rawYaml     :: ByteString
      }
  deriving (Eq, Ord, Show)

instance HasField "name" Recipe Name where
  getField = _name

instance HasField "ingredients" Recipe [IngDesc] where
  getField = _ingredients

instance HasField "description" Recipe Desc where
  getField = _description

instance HasField "references" Recipe [Ref] where
  getField = _references

instance HasField "rawYaml" Recipe ByteString where
  getField = _rawYaml

-- | Parse a raw YAML document into a 'Recipe'.
parseRecipe :: ByteString -> Either String Recipe
parseRecipe = undefined

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

--------------------------------

-- YAML parser

-- | Recipe structure for YAML encoding.
data YRecipe
  = YRecipe
      { name   :: Text
      , ings   :: Maybe [YIngDesc]
      , desc   :: Maybe Text
      , url    :: Maybe Text
      , source :: Maybe Text
      }
  deriving (Eq, Generic, Ord, Show)

instance FromJSON YRecipe where
  parseJSON = genericParseJSON $ dropLabelOptions 0

instance ToJSON YRecipe where
  toJSON = genericToJSON $ dropLabelOptions 0

data YIngDesc
  = YIngGroup YIngGrouped
  | YIngSingle Text
  deriving (Eq, Generic, Ord, Show)

instance FromJSON YIngDesc where
  parseJSON = genericParseJSON $ dropLabelOptions 0

instance ToJSON YIngDesc where
  toJSON = genericToJSON $ dropLabelOptions 0

data YIngGrouped
  = YIngGrouped
      { g    :: Text
      , ings :: [Text]
      }
  deriving (Eq, Generic, Ord, Show)

instance FromJSON YIngGrouped where
  parseJSON = genericParseJSON $ dropLabelOptions 0

instance ToJSON YIngGrouped where
  toJSON = genericToJSON $ dropLabelOptions 0

toRecipe :: YRecipe -> Either String Recipe
toRecipe yr = do
  igs <- traverse toIngDesc $ maybe [] Prelude.id $ getField @"ings" yr
  return $ Recipe { _name = getField @"name" yr
                  , _ingredients = igs
                  , _description = maybe "" Prelude.id $ getField @"desc" yr
                  , _references = maybe [] return $ toRef (getField @"url" yr) (getField @"source" yr)
                  , _rawYaml = "" -- TODO
                  }

toRef :: Maybe Url -> Maybe Text -> Maybe Ref
toRef mu ms =
  case (mu, ms) of
    (Just u, _)        -> Just $ RefUrl u ms
    (Nothing, Just s)  -> Just $ RefSource s
    (Nothing, Nothing) -> Nothing

toIngDesc :: YIngDesc -> Either String IngDesc
toIngDesc y =
  case y of
    YIngGroup yg -> do
      igs <- traverse parseIngredient $ getField @"ings" yg
      return $ IngGroup (getField @"g" yg) igs
    YIngSingle i -> fmap IngSingle $ parseIngredient i

readYamlFile :: (MonadThrow m, MonadIO m) => FilePath -> m [Recipe]
readYamlFile f = readYaml =<< (liftIO $ BS.readFile f)

loadYamlFile :: (MonadThrow m, MonadIO m) => RecipeStore m -> FilePath -> m [Id]
loadYamlFile rs f = traverse (insertRecipe rs) =<< readYamlFile f

-- | Read a YAML document for multiple 'Recipe's.
readYaml :: (MonadThrow m) => ByteString -> m [Recipe]
readYaml b = do
  yrs <- either throw return $ decodeYAMLDocs b
  either throwString return $ traverse toRecipe yrs


