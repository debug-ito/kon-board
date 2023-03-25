-- | Web application of KonBoard
module KonBoard.Web.App
    ( -- * Web application
      appWith
      -- * KonApp
    , KonApp
    , newKonApp
    , closeKonApp
      -- * Db
    , initDb
    ) where

import           Control.Applicative            ((<$>), (<*>))
import           Control.Exception.Safe         (MonadMask, MonadThrow, bracket, throw, try)
import           Control.Monad                  (forM_, when)
import           Control.Monad.Except           (ExceptT (..))
import           Control.Monad.Logger           (LoggingT, MonadLogger, logDebugN, logInfoN,
                                                 runStderrLoggingT)
import           Control.Monad.Trans            (MonadIO (liftIO))
import           Data.Monoid                    ((<>))
import           Data.Proxy                     (Proxy (..))
import           Data.Text                      (pack)
import qualified Data.Text.Lazy                 as TL
import qualified Data.Text.Lazy.Encoding        as TL
import           GHC.Records                    (HasField (..))
import           Network.Wai.Middleware.Rewrite (rewritePureWithQueries)
import qualified Servant                        as Sv
import           Servant                        (Application, Handler (..), Raw,
                                                 ServerError (errBody), hoistServer, (:<|>) (..),
                                                 (:>))
import           System.Directory               (doesFileExist, removeFile)
import           System.FilePath.Glob           (glob)

import           KonBoard.Bridge.MealPlan       (BMealPlan, toBMealPlan)
import           KonBoard.Bridge.Recipe         (BRecipeId, BRecipeStored, fromBRecipeId,
                                                 toBRecipeStored)
import           KonBoard.Bridge.Time           (BDay, fromBDay)
import qualified KonBoard.Db                    as Db
import           KonBoard.MealPlan              (MealPlanStore (..))
import qualified KonBoard.MealPlan.Yaml         as MealPlanY
import           KonBoard.Recipe                (RecipeStore (..))
import qualified KonBoard.Recipe.Yaml           as RecipeY
import           KonBoard.Web.Api               (DataApi)

type AppApi = DataApi
              :<|> "static" :> Raw

-- | KonBoard Web application
data KonApp m
  = KonApp
      { mealPlanStore :: MealPlanStore m
      , recipeStore   :: RecipeStore m
      , dirStatic     :: FilePath
      , dbConn        :: Db.Conn
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
  fmap (map toBMealPlan)$ getMealPlans store start end

handleGetRecipe :: MonadThrow m
                => RecipeStore m
                -> BRecipeId
                -> m BRecipeStored
handleGetRecipe rstore rid = do
  mr <- getRecipeById rstore $ fromBRecipeId rid
  maybe (throw Sv.err404) (return . toBRecipeStored) mr

appToHandler :: AppM a -> Handler a
appToHandler app = Handler $ ExceptT $ try app

-- | Make 'Application' from 'Server'.
appWith :: KonApp AppM -> Application
appWith konApp = application
  where
    application = rewriteRoot $ Sv.serve api $ hoistServer api appToHandler service
    api = Proxy :: Proxy AppApi
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

dbFile :: FilePath
dbFile = "kon-board.sqlite3"

newKonApp :: LoggingT IO (KonApp AppM)
newKonApp =  do
  conn <- Db.newSqliteConn dbFile
  return $ KonApp { mealPlanStore = Db.mealPlanStoreDb conn
                  , recipeStore = Db.recipeStoreDb conn
                  , dirStatic = "static"
                  , dbConn = conn
                  }

closeKonApp :: MonadIO m1 => KonApp m2 -> m1 ()
closeKonApp app = liftIO $ Db.close $ getField @"dbConn" app

initDb :: (MonadLogger m, MonadIO m, MonadMask m) => m ()
initDb = do
  clearDb
  bracket (Db.newSqliteConn dbFile) Db.close $ \conn -> do
    let recipeS = Db.recipeStoreDb conn
    loadRecipes recipeS
    loadMealPlans recipeS $ Db.mealPlanStoreDb conn
  where
    clearDb = do
      exists <- liftIO $ doesFileExist dbFile
      when exists $ do
        logDebugN (pack dbFile <> " exists. Remove it.")
        liftIO $ removeFile dbFile
    loadRecipes recipeS = do
      recipeFiles <- liftIO $ glob "recipes/*.yaml"
      forM_ recipeFiles $ \recipeFile -> do
        logDebugN ("Load recipe file: " <> pack recipeFile)
        liftIO $ RecipeY.loadYamlFile recipeS recipeFile
    loadMealPlans recipeS mealPlanS = do
      mealPlanFiles <- liftIO $ glob "meal-plans/*.yaml"
      forM_ mealPlanFiles $ \mealPlanFile -> do
        logDebugN ("Load meal plan file: " <> pack mealPlanFile)
        liftIO $ MealPlanY.loadYamlFile mealPlanS recipeS mealPlanFile
