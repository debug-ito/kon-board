module KonBoard.MealPlan.Memory
    ( mealPlanStoreMemory
    ) where

import qualified Data.Map.Strict   as M
import           Data.Time         (Day)

import           KonBoard.Base     (HasField (..), MonadIO, liftIO, modifyIORef, newIORef,
                                    readIORef)
import           KonBoard.MealPlan (MealPhase, MealPlan (..), MealPlanStore (..))

mealPlanStoreMemory :: (MonadIO m1, MonadIO m2) => m1 (MealPlanStore m2)
mealPlanStoreMemory = do
  refM <- liftIO $ newIORef M.empty
  let putMP m = liftIO $ modifyIORef refM $ putMealPlanPure m
      searchMP s e = liftIO $ searchMealPlansPure s e <$> readIORef refM
  return $ MealPlanStore { putMealPlan = putMP
                         , searchMealPlans = searchMP
                         }

type MealPlanMap = M.Map Key MealPlan

type Key = (Day, MealPhase)

mkKey :: MealPlan -> Key
mkKey mp = (getField @"day" mp, getField @"phase" mp)

putMealPlanPure :: MealPlan -> MealPlanMap -> MealPlanMap
putMealPlanPure mp = M.insert (mkKey mp) mp

searchMealPlansPure :: Day -> Day -> MealPlanMap -> [MealPlan]
searchMealPlansPure start end mpm = M.elems $ M.takeWhileAntitone endCondition $ M.dropWhileAntitone startCondition mpm
  where
    startCondition (d, _) = d < start
    endCondition (d, _) = d < end
