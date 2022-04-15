-- | YAML decoder for MealPlans
module KonBoard.MealPlan.Yaml
    ( readYaml
    , readYamlFile
    , loadYamlFile
    ) where

import qualified Data.ByteString   as BS

import           KonBoard.Base     (ByteString, MonadIO (..), MonadLogger, MonadThrow, traverse_)
import           KonBoard.MealPlan (MealPlan, MealPlanStore (..))
import           KonBoard.Recipe   (RecipeStore (..))

readYamlFile :: (MonadLogger m, MonadThrow m, MonadIO m) => RecipeStore m -> FilePath -> m [MealPlan]
readYamlFile r f = readYaml r =<< (liftIO $ BS.readFile f)

loadYamlFile :: (MonadLogger m, MonadThrow m, MonadIO m) =>  MealPlanStore m -> RecipeStore m -> FilePath -> m ()
loadYamlFile m r f  = traverse_ (putMealPlan m) =<< readYamlFile r f

readYaml :: (MonadLogger m, MonadThrow m) => RecipeStore m -> ByteString -> m [MealPlan]
readYaml = undefined -- TODO

