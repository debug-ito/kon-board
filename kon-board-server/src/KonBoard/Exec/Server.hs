-- | Executable of kon-board backend server
module KonBoard.Exec.Server
    ( main
    ) where

import           Control.Monad.Logger     (logInfoN, runStderrLoggingT)
import           Data.Monoid              ((<>))
import           Data.Text                (pack)
import           Network.Wai.Handler.Warp (run)

import           KonBoard.Web.App         (appWith, makeDefaultKonApp)

main :: IO ()
main = do
  server <- runStderrLoggingT $ makeDefaultKonApp
  let port = 8888 :: Int
  runStderrLoggingT $ do
    logInfoN ("Listen on port " <> (pack $ show port))
  run port $ appWith server

