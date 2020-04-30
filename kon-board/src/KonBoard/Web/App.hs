{-# LANGUAGE GADTs #-}
-- |
-- Module: KonBoard.Web.App
-- Description: Web application of KonBoard
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module KonBoard.Web.App
  ( appWith,
    Server(..)
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.Except (throwError)
import Control.Monad.Trans (liftIO)
import Data.Proxy (Proxy(..))
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Servant
  ( Application, Handler,
    ServantErr(errBody)
  )
import qualified Servant as Sv

import KonBoard.Bridge.Time (BDay, fromBDay)
import KonBoard.Bridge.MealPlan (BMealPlan, toBMealPlan)
import KonBoard.MealPlan.Store (AMealPlanStore(..))
import KonBoard.Web.API (DataAPI)

-- | Everything you need run the Web application.
data Server where
  Server ::
    ( AMealPlanStore s
    )=>
    { sMealPlanStore :: s
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
appWith Server { sMealPlanStore = mp_store } = Sv.serve api service
  where
    api = Proxy :: Proxy DataAPI
    service = handleGetMealPlans mp_store
