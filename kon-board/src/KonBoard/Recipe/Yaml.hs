-- | YAML decoder for Recipes
module KonBoard.Recipe.Yaml
    ( readYaml
    , readYamlFile
    , loadYamlFile
    ) where

import qualified Data.ByteString as BS

import           KonBoard.Base   (ByteString, Generic, MonadIO, MonadLogger, MonadThrow, Text,
                                  liftIO)
import           KonBoard.Recipe (Id, Recipe, RecipeStore (..))

readYamlFile :: (MonadLogger m, MonadThrow m, MonadIO m) => FilePath -> m [Recipe]
readYamlFile f = readYaml =<< (liftIO $ BS.readFile f)

loadYamlFile :: (MonadLogger m, MonadThrow m, MonadIO m) => RecipeStore m -> FilePath -> m [Id]
loadYamlFile rs f = traverse (putRecipe rs) =<< readYamlFile f

readYaml :: (MonadLogger m, MonadThrow m) => ByteString -> m [Recipe]
readYaml = undefined -- TODO

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
