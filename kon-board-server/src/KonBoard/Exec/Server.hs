-- | Executable of kon-board backend server
module KonBoard.Exec.Server
    ( main
    , initDbMain
    ) where

import           Control.Exception.Safe   (bracket)
import           Control.Monad            (when)
import           Control.Monad.IO.Class   (MonadIO (..))
import           Control.Monad.Logger     (logInfoN, runStderrLoggingT)
import           Data.Monoid              ((<>))
import           Data.Text                (pack)
import           Network.Wai.Handler.Warp (run)
import           System.Environment       (lookupEnv)

import           KonBoard.Web.App         (appWith, closeKonApp, initDb, newKonApp)

main :: IO ()
main = runStderrLoggingT $ do
  let noInitEnvName = "KON_BOARD_NO_INIT_DB_AT_START"
  noInitAtStart <- liftIO $ fmap (maybe "" id) $ lookupEnv noInitEnvName
  if (noInitAtStart == "")
    then initDb
    else logInfoN ("environment variable " <> pack noInitEnvName <> " is set. Skip initializing the DB...")
  bracket newKonApp closeKonApp $ \server -> do
    let port = 8888 :: Int
    logInfoN ("Listen on port " <> (pack $ show port))
    liftIO $ run port $ appWith server

initDbMain :: IO ()
initDbMain = runStderrLoggingT initDb

