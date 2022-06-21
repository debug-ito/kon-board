-- | Web application of KonBoard
module KonBoard.Web.App
    ( -- * Application
      appWith
      -- * Server
    , Server (..)
    , makeDefaultServer
    , runLogging
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
import           Network.Wai.Middleware.Rewrite (rewritePureWithQueries)
import           Servant                        (Application, Handler, Raw, ServerError (errBody),
                                                 (:<|>) (..), (:>))
import qualified Servant                        as Sv
import           System.FilePath.Glob           (glob)

import           KonBoard.Bridge.MealPlan       (BMealPlan, toBMealPlan)
import           KonBoard.Bridge.Recipe         (BRecipe, BRecipeID, fromBRecipeID, toBRecipe)
import           KonBoard.Bridge.Time           (BDay, fromBDay)
import           KonBoard.MealPlan              (MealPlanStore (..))
import           KonBoard.Recipe                (RecipeStore (..))
import qualified KonBoard.Recipe.Store          as Recipe
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

handleGetRecipe :: RecipeStore
                -> BRecipeID
                -> Handler BRecipe
handleGetRecipe rstore rid = fmap toBRecipe $ liftIO $ loadRecipe rstore $ fromBRecipeID rid

-- | Make 'Application' from 'Server'.
appWith :: Server -> Application
appWith Server { sMealPlanStore = mp_store,
                 sRecipeStore = r_store,
                 sDirStatic = dir_static
               } = application
  where
    application = rewriteRoot $ Sv.serve api service
    api = Proxy :: Proxy AppAPI
    service = ( handleGetMealPlans mp_store
                :<|> handleGetRecipe r_store
              )
              :<|> Sv.serveDirectoryWebApp dir_static
    rewriteRoot = rewritePureWithQueries rewrite
      where
        index_page = ["static", "index.html"]
        rewrite ([], q) _                  = (index_page, q)
        rewrite (("recipes" : _ : _), q) _ = (index_page, q)
        rewrite (("days" : _), q) _        = (index_page, q)
        rewrite pq _                       = pq

makeDefaultServer :: IO Server
makeDefaultServer = runStderrLoggingT makeDefaultServer'

runLogging :: MonadIO m => Server -> LoggingT m a -> m a
runLogging _ = runStderrLoggingT

makeDefaultServer' :: LoggingT IO Server
makeDefaultServer' =  do
  recipe_files <- liftIO $ glob "recipes/*.yaml"
  logDebugN ("Load recipe files: " <> (pack $ show recipe_files))
  mealplan_files <- liftIO $ glob "meal-plans/*.yaml"
  logDebugN ("Load meal plan files: " <> (pack $ show mealplan_files))
  rstore <- Recipe.openYAMLs recipe_files
  mstore <- MealPlan.openYAMLs rstore mealplan_files
  return $ Server { sMealPlanStore = mstore,
                    sRecipeStore = rstore,
                    sDirStatic = "static"
                  }
