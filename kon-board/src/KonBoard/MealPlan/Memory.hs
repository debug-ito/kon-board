module KonBoard.MealPlan.Memory
    ( MealPlanStoreMemory
    , mealPlanStoreMemory
    , readYaml
    ) where

import           KonBoard.Base     (ByteString, MonadLogger, MonadThrow, Monoid (..),
                                    Semigroup (..))
import           KonBoard.MealPlan (MealPlan, MealPlanStore)
import           KonBoard.Recipe   (RecipeStore)

-- | On-memory 'MealPlanStore'.
newtype MealPlanStoreMemory
  = MealPlanStoreMemory [MealPlan]
  deriving (Eq, Monoid, Semigroup, Show)

mealPlanStoreMemory :: Applicative m => MealPlanStoreMemory -> MealPlanStore m
mealPlanStoreMemory = undefined -- TODO

readYaml :: (MonadLogger m, MonadThrow m) => RecipeStore m -> ByteString -> m MealPlanStoreMemory
readYaml = undefined -- TODO


---- -- | Common API of 'MealPlan' store.
---- class AMealPlanStore s where
----   searchMealPlans :: s -- ^ 'MealPlan' store
----                   -> Day -- ^ start date (inclusive)
----                   -> Day -- ^ end date (exclusive)
----                   -> IO [MealPlan]
----
---- -- | 'MealPlan' store based on YAML files.
---- newtype YAMLStore
----   = YAMLStore [MealPlan]
----
---- instance AMealPlanStore YAMLStore where
----   searchMealPlans (YAMLStore mps) start end = return $ sort $ filter isInside mps
----     where
----       isInside mp = start <= mp_day && mp_day < end
----         where
----           mp_day = mealDay mp
----
---- -- | Read YAML files to make a 'YAMLStore'.
---- openYAMLs :: (MonadLogger m, MonadIO m) => RecipeStore -> [FilePath] -> m YAMLStore
---- openYAMLs rstore files = do
----   yaml_docs <- fmap concat $ forM files $ \file -> do
----     logInfoN ("Read meal plan YAML file: " <> pack file)
----     liftIO $ readYAMLDocs file
----   liftIO $ fmap (YAMLStore . concat) $ traverse (fromMonthPlan rstore) $ yaml_docs
----
---- aesonOpt :: Aeson.Options
---- aesonOpt =
----   Aeson.defaultOptions
----   { Aeson.fieldLabelModifier = lmod,
----     Aeson.omitNothingFields = True
----   }
----   where
----     lmod label = drop 3 label
----
---- fromMonthPlan :: RecipeStore -> MonthPlan -> IO [MealPlan]
---- fromMonthPlan rstore mp = traverse toMP $ mp_plan mp
----   where
----     toMP dp = do
----       rsummaries <- traverse (loadRecipeByName' rstore) $ fromAOSToList $ dp_m dp
----       return $ MealPlan
----                { mealDay = fromGregorian (mp_year mp) (mp_month mp) (dp_d dp),
----                  mealPhase = unYMealPhase $ dp_p dp,
----                  mealRecipes = rsummaries,
----                  mealNotes = fromAOSToList $ dp_n dp
----                }
----
---- fromAOSToList :: Maybe (ArrayOrSingle a) -> [a]
---- fromAOSToList = maybe [] F.toList
----
---- data MonthPlan
----   = MonthPlan
----       { mp_year  :: Integer
----       , mp_month :: Int
----       , mp_plan  :: [DayPlan]
----       }
----   deriving (Eq, Generic, Ord, Show)
----
---- instance FromJSON MonthPlan where
----   parseJSON = Aeson.genericParseJSON aesonOpt
----
---- instance ToJSON MonthPlan where
----   toJSON = Aeson.genericToJSON aesonOpt
----   toEncoding = Aeson.genericToEncoding aesonOpt
----
---- type Meals = ArrayOrSingle Name
----
---- type Notes = ArrayOrSingle Note
----
---- data DayPlan
----   = DayPlan
----       { dp_d :: Int
----       , dp_p :: YMealPhase
----       , dp_m :: Maybe Meals
----       , dp_n :: Maybe Notes
----       }
----   deriving (Eq, Generic, Ord, Show)
----
---- instance FromJSON DayPlan where
----   parseJSON = Aeson.genericParseJSON aesonOpt
----
---- instance ToJSON DayPlan where
----   toJSON = Aeson.genericToJSON aesonOpt
----   toEncoding = Aeson.genericToEncoding aesonOpt
----
---- -- | MealPhase for YAML encoding.
---- newtype YMealPhase
----   = YMealPhase { unYMealPhase :: MealPhase }
----   deriving (Eq, Ord, Show)
----
---- instance FromJSON YMealPhase where
----   parseJSON v = fmap YMealPhase $ parsedMealPhase
----     where
----       parsedMealPhase =
----         case v of
----           Aeson.String s -> either fail return $ toMealPhase s
----           Aeson.Array a -> do
----             guard (F.length a == 1)
----             case F.toList a of
----               [(Aeson.String p)] -> return $ MealOther p
----               _                  -> empty
----           _ -> empty
----
---- instance ToJSON YMealPhase where
----   toJSON (YMealPhase mp) =
----     case mp of
----       MealOther p -> toJSON [p]
----       _           -> Aeson.String $ fromMealPhase mp


