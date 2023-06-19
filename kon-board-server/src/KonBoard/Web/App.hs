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
import           Data.Pool                      (Pool, defaultPoolConfig, destroyAllResources,
                                                 newPool, withResource)
import           Data.Proxy                     (Proxy (..))
import           Data.Text                      (Text, pack)
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
import           KonBoard.Bridge.Recipe         (BAnswerRecipe, BRecipeId, BRecipeStored,
                                                 fromBRecipeId, toBAnswerRecipe, toBRecipeStored)
import           KonBoard.Bridge.Time           (BDay, fromBDay)
import qualified KonBoard.Db                    as Db
import           KonBoard.MealPlan              (MealPlanStore (..))
import qualified KonBoard.MealPlan.Yaml         as MealPlanY
import           KonBoard.Query                 (Query (..))
import           KonBoard.Recipe                (RecipeStore (..))
import qualified KonBoard.Recipe.Yaml           as RecipeY
import           KonBoard.Web.Api               (DataApi)

type AppApi = DataApi
              :<|> "static" :> Raw

-- | KonBoard Web application
data KonApp
  = KonApp
      { dirStatic :: FilePath
      , dbPool    :: Pool Db.Conn
      }

-- | The application monad
type AppM = IO

serverErrorOnLeft :: MonadThrow m => ServerError -> Either e a -> m a
serverErrorOnLeft err = either (const $ throw err) return

handleGetMealPlans :: Pool Db.Conn
                   -> BDay -- ^ start
                   -> BDay -- ^ end
                   -> IO [BMealPlan]
handleGetMealPlans pool bs be = withResource pool go
  where
    go conn = do
      let store = Db.mealPlanStoreDb conn
      (start, end) <- serverErrorOnLeft Sv.err400 $ (,) <$> (fromBDay bs) <*> (fromBDay be)
      fmap (map toBMealPlan)$ getMealPlans store start end

handleGetRecipe :: Pool Db.Conn
                -> BRecipeId
                -> IO BRecipeStored
handleGetRecipe pool rid = withResource pool go
  where
    go conn = do
      let store = Db.recipeStoreDb conn
      mr <- getRecipeById store $ fromBRecipeId rid
      maybe (throw Sv.err404) (return . toBRecipeStored) mr

handleGetRecipesByQuery :: Pool Db.Conn -> Maybe Text -> Maybe Int -> Maybe Int -> IO BAnswerRecipe
handleGetRecipesByQuery pool inQ inCount inOffset = withResource pool go
  where
    go conn = do
      let rStore = Db.recipeStoreDb conn
      case (mCount, mOffset) of
        (Just c, Just o) -> fmap toBAnswerRecipe $ getRecipesByQuery rStore (Query { query =  q, count = c, offset = o})
        _ -> throw Sv.err400
    q = maybe "" id inQ
    mCount = nonNegative $ maybe 20 id inCount
    mOffset = nonNegative $ maybe 0 id inOffset

nonNegative :: Int -> Maybe Word
nonNegative i = if i >= 0
                then Just $ fromIntegral i
                else Nothing

appToHandler :: AppM a -> Handler a
appToHandler app = Handler $ ExceptT $ try app

-- | Make 'Application' from 'Server'.
appWith :: KonApp -> Application
appWith konApp = application
  where
    application = rewriteRoot $ Sv.serve api $ hoistServer api appToHandler service
    api = Proxy :: Proxy AppApi
    dbP = getField @"dbPool" konApp
    service = ( handleGetMealPlans dbP
                :<|> ( handleGetRecipe dbP
                       :<|> handleGetRecipesByQuery dbP
                     )
              )
              :<|> Sv.serveDirectoryWebApp (getField @"dirStatic" konApp)
    rewriteRoot = rewritePureWithQueries rewrite
      where
        index_page = ["static", "index.html"]
        rewrite ([], q) _                  = (index_page, q)
        rewrite (("recipes" : _ : _), q) _ = (index_page, q)
        rewrite (("days" : _), q) _        = (index_page, q)
        rewrite pq _                       = pq

newKonApp :: FilePath -> LoggingT IO KonApp
newKonApp dbFile =  do
  logDebugN ("Use the DB at " <> pack dbFile)
  liftIO $ bracket (Db.newSqliteConn dbFile) Db.close Db.initDb
  let poolConf = defaultPoolConfig (Db.newSqliteConn dbFile) Db.close openTimeSec maxConnNum
      openTimeSec = 3600.0
      maxConnNum = 10
  p <- liftIO $ newPool poolConf
  return $ KonApp { dirStatic = "static"
                  , dbPool = p
                  }

closeKonApp :: MonadIO m => KonApp -> m ()
closeKonApp app = liftIO $ destroyAllResources $ getField @"dbPool" app

initDb :: (MonadLogger m, MonadIO m, MonadMask m) => FilePath -> m ()
initDb dbFile = do
  clearDb
  bracket (Db.newSqliteConn dbFile) Db.close $ \conn -> do
    Db.initDb conn
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
