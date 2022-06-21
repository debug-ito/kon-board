-- | Web application of KonBoard
module KonBoard.Web.App
    ( -- * Web application
      appWith
      -- * KonApp
    , KonApp
    , makeDefaultKonApp
    ) where

import           Control.Applicative            ((<$>), (<*>))
import           Control.Exception.Safe         (MonadThrow (..))
import           Control.Monad.Logger           (LoggingT, logDebugN, logInfoN, runStderrLoggingT)
import           Control.Monad.Trans            (MonadIO (liftIO))
import           Data.Monoid                    ((<>))
import           Data.Proxy                     (Proxy (..))
import           Data.Text                      (pack)
import qualified Data.Text.Lazy                 as TL
import qualified Data.Text.Lazy.Encoding        as TL
import           GHC.Records                    (HasField (..))
import           Network.Wai.Middleware.Rewrite (rewritePureWithQueries)
import           Servant                        (Application, Handler, Raw, ServerError (errBody),
                                                 hoistServer, (:<|>) (..), (:>))
import qualified Servant                        as Sv
import           System.FilePath.Glob           (glob)

import           KonBoard.Bridge.MealPlan       (BMealPlan, toBMealPlan)
import           KonBoard.Bridge.Recipe         (BRecipeId, BRecipeStored, fromBRecipeId,
                                                 toBRecipeStored)
import           KonBoard.Bridge.Time           (BDay, fromBDay)
import           KonBoard.MealPlan              (MealPlanStore (..))
import qualified KonBoard.MealPlan.Yaml         as MealPlanY
import           KonBoard.Recipe                (RecipeStore (..))
import           KonBoard.Recipe.Memory         (recipeStoreMemory)
import qualified KonBoard.Recipe.Yaml           as RecipeY
import           KonBoard.Web.API               (DataAPI)

type AppAPI = DataAPI
              :<|> "static" :> Raw

-- | KonBoard Web application
data KonApp m
  = KonApp
      { mealPlanStore :: MealPlanStore m
      , recipeStore   :: RecipeStore m
      , dirStatic     :: FilePath
      }

-- | The application monad
type AppM = IO

serverErrorOnLeft :: MonadThrow m => ServerError -> Either e a -> m a
serverErrorOnLeft err = either (const $ throw err) return

handleGetMealPlans :: MonadThrow m
                   => MealPlanStore m
                   -> BDay -- ^ start
                   -> BDay -- ^ end
                   -> m [BMealPlan]
handleGetMealPlans store bs be = do
  (start, end) <- serverErrorOnLeft Sv.err400 $ (,) <$> (fromBDay bs) <*> (fromBDay be)
  fmap (map toBMealPlan)$ searchMealPlans store start end

handleGetRecipe :: MonadThrow m
                => RecipeStore m
                -> BRecipeId
                -> m BRecipeStored
handleGetRecipe rstore rid = do
  mr <- getRecipeById rstore $ fromBRecipeId rid
  maybe (throw Sv.err404) (return . toBRecipeStored) mr

appToHandler :: AppM a -> Handler a
appToHandler = undefined -- TODO

-- | Make 'Application' from 'Server'.
appWith :: KonApp AppM -> Application
appWith konApp = application
  where
    application = rewriteRoot $ Sv.serve api $ hoistServer api appToHandler service
    api = Proxy :: Proxy AppAPI
    service = ( handleGetMealPlans (getField @"mealPlanStore" konApp)
                :<|> handleGetRecipe (getField @"recipeStore" konApp)
              )
              :<|> Sv.serveDirectoryWebApp (getField @"dirStatic" konApp)
    rewriteRoot = rewritePureWithQueries rewrite
      where
        index_page = ["static", "index.html"]
        rewrite ([], q) _                  = (index_page, q)
        rewrite (("recipes" : _ : _), q) _ = (index_page, q)
        rewrite (("days" : _), q) _        = (index_page, q)
        rewrite pq _                       = pq

makeDefaultKonApp :: LoggingT IO (KonApp AppM)
makeDefaultKonApp =  do
  recipeS <- recipeStoreMemory
  recipe_files <- liftIO $ glob "recipes/*.yaml"
  logDebugN ("Load recipe files: " <> (pack $ show recipe_files))
  -- TODO: load Recipe YAML files
  mealplan_files <- liftIO $ glob "meal-plans/*.yaml"
  logDebugN ("Load meal plan files: " <> (pack $ show mealplan_files))
  -- TODO: make MealPlanStore.Memory and load YAML files.

  -- TODO: rewrite below
  rstore <- Recipe.openYAMLs recipe_files
  mstore <- MealPlan.openYAMLs rstore mealplan_files
  return $ Server { sMealPlanStore = mstore,
                    sRecipeStore = rstore,
                    sDirStatic = "static"
                  }
