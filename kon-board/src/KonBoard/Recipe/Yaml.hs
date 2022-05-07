-- | YAML decoder for Recipes
module KonBoard.Recipe.Yaml
    ( readYaml
    , readYamlFile
    , loadYamlFile
    ) where

import           Data.Aeson          (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON)
import qualified Data.ByteString     as BS

import           KonBoard.Base       (ByteString, Generic, HasField (..), MonadIO, MonadLogger,
                                      MonadThrow, Text, liftIO, traverse)
import           KonBoard.Recipe     (Id, IngDesc (..), Recipe (..), RecipeStore (..), Ref (..),
                                      Url, parseIngredient)
import           KonBoard.Util.Aeson (dropLabelOptions)

readYamlFile :: (MonadLogger m, MonadThrow m, MonadIO m) => FilePath -> m [Recipe]
readYamlFile f = readYaml =<< (liftIO $ BS.readFile f)

loadYamlFile :: (MonadLogger m, MonadThrow m, MonadIO m) => RecipeStore m -> FilePath -> m [Id]
loadYamlFile rs f = traverse (putRecipe rs) =<< readYamlFile f

readYaml :: (MonadLogger m, MonadThrow m) => ByteString -> m [Recipe]
readYaml = undefined -- TODO

toRecipe :: YRecipe -> Either String Recipe
toRecipe yr = do
  ings <- traverse toIngDesc $ maybe [] id $ getField @"ings" yr
  return $ Recipe { name = getField @"name" yr
                  , ingredients = ings
                  , description = maybe "" id $ getField @"desc" yr
                  , references = maybe [] return $ toRef (getField @"url" yr) (getField @"source" yr)
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
      ings <- traverse parseIngredient $ getField @"ings" yg
      return $ IngGroup (getField @"g" yg) ings
    YIngSingle i -> fmap IngSingle $ parseIngredient i

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
