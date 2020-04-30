module Main (main) where

import Network.Wai.Handler.Warp (run)

import KonBoard.Web.App (appWith, makeDefaultServer)

main :: IO ()
main = run 8888 =<< (fmap appWith $ makeDefaultServer)
