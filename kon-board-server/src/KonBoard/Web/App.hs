-- | Web application of KonBoard
module KonBoard.Web.App
    ( -- * Web application
      appWith
      -- * KonApp
    , KonApp
    , newKonApp
    , closeKonApp
    ) where

import           Control.Applicative            ((<$>), (<*>))
import           Control.Exception.Safe         (MonadThrow, throw, try)
import           Control.Monad                  (forM_, when)
import           Control.Monad.Except           (ExceptT (..))
import           Control.Monad.Logger           (LoggingT, logDebugN, logInfoN, runStderrLoggingT)
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

newKonApp :: LoggingT IO (KonApp AppM)
newKonApp =  do
  conn <- initConn
  let recipeS = Db.recipeStoreDb conn
      mealplanS = Db.mealPlanStoreDb conn
  recipeFiles <- liftIO $ glob "recipes/*.yaml"
  forM_ recipeFiles $ \recipeFile -> do
    logDebugN ("Load recipe file: " <> pack recipeFile)
    liftIO $ RecipeY.loadYamlFile recipeS recipeFile
  mealplanFiles <- liftIO $ glob "meal-plans/*.yaml"
  forM_ mealplanFiles $ \mealplanFile -> do
    logDebugN ("Load meal plan file: " <> pack mealplanFile)
    liftIO $ MealPlanY.loadYamlFile mealplanS recipeS mealplanFile
  return $ KonApp { mealPlanStore = mealplanS
                  , recipeStore = recipeS
                  , dirStatic = "static"
                  , dbConn = conn
                  }
  where
    initConn = do
      let dbFile = "kon-board.sqlite3"
      exists <- liftIO $ doesFileExist dbFile
      when exists $ do
        liftIO $ removeFile dbFile
      Db.newSqliteConn dbFile

closeKonApp :: MonadIO m1 => KonApp m2 -> m1 ()
closeKonApp app = liftIO $ Db.close $ getField @"dbConn" app
