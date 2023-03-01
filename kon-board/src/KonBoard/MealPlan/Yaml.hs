-- | YAML decoder for MealPlans
module KonBoard.MealPlan.Yaml
    ( readYaml
    , readYamlFile
    , loadYamlFile
    ) where

import qualified Data.ByteString     as BS
import           Data.Foldable       (foldr, toList)
import qualified Data.Text           as T
import           Data.Time           (fromGregorian)

import           KonBoard.Base       (ByteString, FromJSON (..), Generic, HasField (..),
                                      MonadIO (..), MonadThrow, Text, ToJSON (..), genericParseJSON,
                                      genericToJSON, throw, throwString, traverse_)
import           KonBoard.MealPlan   (MealPhase (..), MealPlan (..), MealPlanStore (..),
                                      toMealPhase)
import           KonBoard.Recipe     (RecipeStore (..), RecipeStored (..))
import           KonBoard.Util.Aeson (dropLabelOptions)
import           KonBoard.Util.Yaml  (ArrayOrSingle (..), decodeYaml, splitYamlDocs)

readYamlFile :: (MonadThrow m, MonadIO m) => RecipeStore m -> FilePath -> m [MealPlan RecipeStored]
readYamlFile r f = readYaml r =<< (liftIO $ BS.readFile f)

loadYamlFile :: (MonadThrow m, MonadIO m) =>  MealPlanStore m -> RecipeStore m -> FilePath -> m ()
loadYamlFile mps r f  = traverse_ (putMealPlan mps . fmap (getField @"id")) =<< readYamlFile r f

readYaml :: (MonadThrow m) => RecipeStore m -> ByteString -> m [MealPlan RecipeStored]
readYaml rs rawDoc = do
  ymps <- either throwString return $ traverse decodeYaml $ splitYamlDocs rawDoc
  fmap concat $ traverse (either throwString return) =<< (traverse (fromYMealPlan rs) $ ymps)

fromYMealPlan :: Monad m => RecipeStore m -> YMealPlan -> m (Either String [MealPlan RecipeStored])
fromYMealPlan rs ymp = fmap accumErrors $ traverse fromYDayPlan' $ getField @"plan" ymp
  where
    fromYDayPlan' = fromYDayPlan rs (getField @"year" ymp) (getField @"month" ymp)

fromYDayPlan :: Monad m => RecipeStore m -> Integer -> Int -> YDayPlan -> m (Either String (MealPlan RecipeStored))
fromYDayPlan rs y mon ydp = do
  eRecipes <- fmap accumErrors $ traverse getRecipeByName' $ flatten $ getField @"m" ydp
  return $ mkMealPlan <$> eRecipes <*> (fromYMealPhase $ getField @"p" ydp)
  where
    getRecipeByName' name = fmap (maybe (Left ("Cannot find recipe: " <> T.unpack name)) Right) $ getRecipeByName rs name
    flatten = concat . toList . fmap toList
    mkMealPlan rcps ph =
      MealPlan
      { day = fromGregorian y mon $ getField @"d" ydp
      , phase = ph
      , recipes = rcps
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
      { year  :: Integer
      , month :: Int
      , plan  :: [YDayPlan]
      }
  deriving (Eq, Generic, Ord, Show)

instance FromJSON YMealPlan where
  parseJSON = genericParseJSON $ dropLabelOptions 0

instance ToJSON YMealPlan where
  toJSON = genericToJSON $ dropLabelOptions 0

data YDayPlan
  = YDayPlan
      { d :: Int
      , p :: YMealPhase
      , m :: Maybe (ArrayOrSingle Text)
      , n :: Maybe (ArrayOrSingle Text)
      }
  deriving (Eq, Generic, Ord, Show)

instance FromJSON YDayPlan where
  parseJSON = genericParseJSON $ dropLabelOptions 0

instance ToJSON YDayPlan where
  toJSON = genericToJSON $ dropLabelOptions 0

type YMealPhase = ArrayOrSingle Text

fromYMealPhase :: YMealPhase -> Either String MealPhase
fromYMealPhase ymp =
  case ymp of
    AOSSingle t -> toMealPhase t
    AOSArray ts ->
      case ts of
        [] -> Left "The meal phase array is empty"
        [t] -> Right $ MealOther t
        _ -> Left ( "The meal phase array has more than one elements: "
                     <> (T.unpack $ T.intercalate ", " ts)
                   )
