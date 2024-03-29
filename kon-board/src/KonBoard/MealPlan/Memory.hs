module KonBoard.MealPlan.Memory
    ( newMealPlanStore
    ) where

import qualified Data.Map.Strict   as M
import qualified Data.Text         as T
import           Data.Time         (Day)

import           KonBoard.Base     (HasField (..), MonadIO, MonadThrow, liftIO, modifyIORef,
                                    newIORef, readIORef, throwString)
import           KonBoard.MealPlan (MealPhase, MealPlan (..), MealPlanStore (..))
import           KonBoard.Recipe   (Id, RecipeStore (getRecipeById), RecipeStored)

newMealPlanStore :: (MonadIO m1, MonadIO m2, MonadThrow m2) => RecipeStore m2 -> m1 (MealPlanStore m2)
newMealPlanStore rStore = do
  refM <- liftIO $ newIORef M.empty
  let putMP m = liftIO $ modifyIORef refM $ putMealPlanImpl m
      getMP s e = getMealPlansImpl rStore s e =<< (liftIO $ readIORef refM)
  return $ MealPlanStore { putMealPlan = putMP
                         , getMealPlans = getMP
                         }

type MealPlanMap = M.Map Key (MealPlan Id)

type Key = (Day, MealPhase)

mkKey :: MealPlan r -> Key
mkKey mp = (getField @"day" mp, getField @"phase" mp)

putMealPlanImpl :: MealPlan Id -> MealPlanMap -> MealPlanMap
putMealPlanImpl mp = M.insert (mkKey mp) mp

getMealPlansImpl :: MonadThrow m => RecipeStore m -> Day -> Day -> MealPlanMap -> m [MealPlan RecipeStored]
getMealPlansImpl rStore start end mpm = traverse (resolveRecipe rStore) $ M.elems $ M.takeWhileAntitone endCondition $ M.dropWhileAntitone startCondition mpm
  where
    startCondition (d, _) = d < start
    endCondition (d, _) = d < end

resolveRecipe :: MonadThrow m => RecipeStore m -> MealPlan Id -> m (MealPlan RecipeStored)
resolveRecipe rStore mp = traverse getRecipeByIdThrowing mp
  where
    getRecipeByIdThrowing i = maybe (throwString ("recipe ID not found: " <> T.unpack i)) return =<< getRecipeById rStore i
