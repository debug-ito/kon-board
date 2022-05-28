-- | YAML decoder for MealPlans
module KonBoard.MealPlan.Yaml
    ( readYaml
    , readYamlFile
    , loadYamlFile
    ) where

import qualified Data.ByteString     as BS
import           Data.Foldable       (foldr, toList)
import qualified Data.Text           as T

import           KonBoard.Base       (ByteString, FromJSON (..), HasField (..), MonadIO (..),
                                      MonadLogger, MonadThrow, Text, ToJSON (..), genericParseJSON,
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

fromYMealPlan :: RecipeStore m -> YMealPlan -> m (Either String [MealPlan])
fromYMealPlan rs ymp = undefined -- TODO

fromYDayPlan :: RecipeStore m -> Int -> Int -> YDayPlan -> m (Either String MealPlan)
fromYDayPlan rs y m ydp = do
  eRecipes <- fmap accumErrors $ traverse getRecipeByName' $ flatten $ getField @"m" ydp
  return $ mkMealPlan <$> eRecipes <*> (toMealPhase $ getField @"p" ydp)
  where
    getRecipeByName' name = fmap (maybe (Left ("Cannot find recipe: " <> T.unpack name)) Right) $ getRecipeByName rs name
    flatten = join . toList . fmap toList
    mkMealPlan rs p =
      MealPlan
      { day = undefined -- TODO
      , phase = p
      , recipes = rs
      , notes = flatten $ getField @"n" ydp
      }

accumErrors :: [Either String a] -> Either String [a]
accumErrors = foldr f (Right [])
  where
    f item acc =
      case (item, acc) of
        (Right i, Right is) -> Right (i : is)
        (Right _, Left e)   -> Left e
        (Left e, Right _)   -> Left e
        (Left e1, Left e2)  -> Left (e1 <> "\n" <> e2)


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

