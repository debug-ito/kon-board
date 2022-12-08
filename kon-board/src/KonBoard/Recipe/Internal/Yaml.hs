-- | __Internal or testing use only.__
module KonBoard.Recipe.Internal.Yaml
    ( parseRecipe
    , readYaml
    , readYamlFile
    , loadYamlFile
    ) where

import           Data.Aeson                    (FromJSON (..), ToJSON (..), genericParseJSON,
                                                genericToJSON)
import qualified Data.ByteString               as BS
import qualified Data.Yaml                     as Yaml
import           KonBoard.Base                 (ByteString, Generic, HasField (..), MonadIO,
                                                MonadThrow, Text, liftIO, throw, throwString,
                                                traverse)
import           KonBoard.Recipe.Internal.Type (Id, IngDesc (..), Recipe (..), RecipeStore (..),
                                                Ref (..), Url, parseIngredient)
import           KonBoard.Util.Aeson           (dropLabelOptions)
import           KonBoard.Util.Yaml            (splitYAMLDocs)

-- | Parse a raw YAML document into a 'Recipe'.
parseRecipe :: ByteString -> Either String Recipe
parseRecipe b = toRecipe b =<< (errToString $ Yaml.decodeEither' b)
  where
    errToString e =
      case e of
        Left er -> Left $ Yaml.prettyPrintParseException er
        Right r -> Right r

readYamlFile :: (MonadThrow m, MonadIO m) => FilePath -> m [Recipe]
readYamlFile f = readYaml =<< (liftIO $ BS.readFile f)

loadYamlFile :: (MonadThrow m, MonadIO m) => RecipeStore m -> FilePath -> m [Id]
loadYamlFile rs f = traverse (insertRecipe rs) =<< readYamlFile f

-- | Read a YAML document for multiple 'Recipe's.
readYaml :: (MonadThrow m) => ByteString -> m [Recipe]
readYaml b = either throwString return $ traverse parseRecipe $ splitYAMLDocs b

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

toRecipe :: ByteString -> YRecipe -> Either String Recipe
toRecipe raw yr = do
  igs <- traverse toIngDesc $ maybe [] Prelude.id $ getField @"ings" yr
  return $ Recipe { _name = getField @"name" yr
                  , _ingredients = igs
                  , _description = maybe "" Prelude.id $ getField @"desc" yr
                  , _references = maybe [] return $ toRef (getField @"url" yr) (getField @"source" yr)
                  , _rawYaml = raw
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
