-- | YAML decoder for MealPlans
module KonBoard.MealPlan.Yaml
    ( readYaml
    , readYamlFile
    , loadYamlFile
    ) where

import qualified Data.ByteString     as BS

import           KonBoard.Base       (ByteString, FromJSON (..), MonadIO (..), MonadLogger,
                                      MonadThrow, Text, ToJSON (..), genericParseJSON,
                                      genericToJSON, traverse_)
import           KonBoard.MealPlan   (MealPlan, MealPlanStore (..))
import           KonBoard.Recipe     (RecipeStore (..))
import           KonBoard.Util.Aeson (dropLabelOptions)
import           KonBoard.Util.Yaml  (ArrayOrSingle (..))

readYamlFile :: (MonadLogger m, MonadThrow m, MonadIO m) => RecipeStore m -> FilePath -> m [MealPlan]
readYamlFile r f = readYaml r =<< (liftIO $ BS.readFile f)

loadYamlFile :: (MonadLogger m, MonadThrow m, MonadIO m) =>  MealPlanStore m -> RecipeStore m -> FilePath -> m ()
loadYamlFile m r f  = traverse_ (putMealPlan m) =<< readYamlFile r f

readYaml :: (MonadLogger m, MonadThrow m) => RecipeStore m -> ByteString -> m [MealPlan]
readYaml = undefined -- TODO

data YMealPlan
  = YMealPlan
      { year  :: Int
      , month :: Int
      , plan  :: [YDayPlan]
      }
  deriving (Eq, Generic, Ord, Show)

instance FromJSON YMealPlan where
  parseJSON = genericParseJSON dropLabelOptions

instance ToJSON YMealPlan where
  toJSON = genericToJSON dropLabelOptions

data YDayPlan
  = YDayPlan
      { d :: Int
      , p :: Text
      , m :: Maybe (ArrayOrSingle Text)
      , n :: Maybe (ArrayOrSingle Text)
      }
  deriving (Eq, Generic, Ord, Show)

instance FromJSON YDayPlan where
  parseJSON = genericParseJSON dropLabelOptions

instance ToJSON YDayPlan where
  toJSON = genericToJSON dropLabelOptions

