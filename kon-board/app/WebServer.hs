module Main (main) where

import Control.Monad.Logger (logInfoN)
import Data.Monoid ((<>))
import Data.Text (pack)
import Network.Wai.Handler.Warp (run)

import KonBoard.Web.App
  (appWith, makeDefaultServer, runLogging)

main :: IO ()
main = do
  server <- makeDefaultServer
  let port = 8888
  runLogging server $ do
    logInfoN ("Listen on port " <> (pack $ show port))
  run 8888 $ appWith server
