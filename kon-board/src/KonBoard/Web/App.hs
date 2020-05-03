{-# LANGUAGE GADTs, TypeOperators, DataKinds, OverloadedStrings #-}
-- |
-- Module: KonBoard.Web.App
-- Description: Web application of KonBoard
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module KonBoard.Web.App
  ( -- * Application
    appWith,
    -- * Server
    Server(..),
    makeDefaultServer,
    runLogging
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.Except (throwError)
import Control.Monad.Logger (LoggingT, runStderrLoggingT, logInfoN, logDebugN)
import Control.Monad.Trans (MonadIO(liftIO))
import Data.Monoid ((<>))
import Data.Proxy (Proxy(..))
import Data.Text (pack)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Network.Wai.Middleware.Rewrite (rewritePureWithQueries)
import Servant
  ( Application, Handler,
    ServantErr(errBody),
    (:>), (:<|>)(..), Raw
  )
import qualified Servant as Sv
import System.FilePath.Glob (glob)

import KonBoard.Bridge.Time (BDay, fromBDay)
import KonBoard.Bridge.MealPlan (BMealPlan, toBMealPlan)
import KonBoard.MealPlan.Store (AMealPlanStore(..))
import qualified KonBoard.MealPlan.Store as MealPlan
import qualified KonBoard.Recipe.Store as Recipe
import KonBoard.Web.API (DataAPI)

type AppAPI = DataAPI
              :<|> "static" :> Raw

-- | Everything you need run the Web application.
data Server where
  Server ::
    ( AMealPlanStore s
    ) =>
    { sMealPlanStore :: s,
      sDirStatic :: FilePath
    } -> Server

toHandler :: Show e => ServantErr -> Either e a -> Handler a
toHandler base_err = either (throwError . mkError) return
  where
    mkError e = base_err { errBody = TL.encodeUtf8 $ TL.pack $ show e }

badReqToHandler :: Show e => Either e a -> Handler a
badReqToHandler = toHandler Sv.err400

handleGetMealPlans :: AMealPlanStore s
                   => s
                   -> BDay -- ^ start
                   -> BDay -- ^ end
                   -> Handler [BMealPlan]
handleGetMealPlans store bs be = do
  (start, end) <- badReqToHandler $ (,) <$> (fromBDay bs) <*> (fromBDay be)
  fmap (map toBMealPlan)$ liftIO $ searchMealPlans store start end

-- | Make 'Application' from 'Server'.
appWith :: Server -> Application
appWith Server { sMealPlanStore = mp_store,
                 sDirStatic = dir_static
               } = application
  where
    application = rewriteRoot $ Sv.serve api service
    api = Proxy :: Proxy AppAPI
    service = handleGetMealPlans mp_store
              :<|> Sv.serveDirectoryWebApp dir_static
    rewriteRoot = rewritePureWithQueries rewrite
      where
        rewrite ([], q) _ = (["static", "index.html"], q)
        rewrite pq _ = pq

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
                    sDirStatic = "static"
                  }
