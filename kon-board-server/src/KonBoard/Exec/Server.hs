-- | Executable of kon-board backend server
module KonBoard.Exec.Server
    ( main
    ) where

import           Control.Exception.Safe   (bracket)
import           Control.Monad.IO.Class   (MonadIO (..))
import           Control.Monad.Logger     (logInfoN, runStderrLoggingT)
import           Data.Monoid              ((<>))
import           Data.Text                (pack)
import           Network.Wai.Handler.Warp (run)

import           KonBoard.Web.App         (appWith, closeKonApp, newKonApp)

main :: IO ()
main = runStderrLoggingT $ do
  bracket newKonApp closeKonApp $ \server -> do
    let port = 8888 :: Int
    logInfoN ("Listen on port " <> (pack $ show port))
    liftIO $ run port $ appWith server

