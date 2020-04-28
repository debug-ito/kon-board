-- |
-- Module: KonBoard.Elm.Generate
-- Description: Generate Elm sources
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module KonBoard.Elm.Generate
  ( main
  ) where

import Data.Proxy (Proxy(..))
import qualified Elm.Module as Elm

import KonBoard.MealPlan (MealPlan)

main :: IO ()
main = putStrLn $ Elm.makeElmModule "MealPlan" $
       [ Elm.DefineElm (Proxy :: Proxy MealPlan)
       ]
