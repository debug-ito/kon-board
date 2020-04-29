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

import KonBoard.Bridge.MealPlan (BMealPlan)
import KonBoard.Bridge.Recipe (BRecipeSummary)

main :: IO ()
main = putStrLn $ Elm.makeElmModule "Bridge" $
       [ Elm.DefineElm (Proxy :: Proxy BMealPlan),
         Elm.DefineElm (Proxy :: Proxy BRecipeSummary)
       ]
