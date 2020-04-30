-- |
-- Module: KonBoard.Exec.ElmGenerate
-- Description: Generate Elm sources
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module KonBoard.Exec.ElmGenerate
  ( main
  ) where

import Data.Proxy (Proxy(..))
import qualified Servant.Elm as Elm
import System.Environment (getArgs)

import KonBoard.Bridge.MealPlan (BMealPlan)
import KonBoard.Bridge.Recipe (BRecipeSummary)
import KonBoard.Web.API (GetMealPlans)

main :: IO ()
main = do
  (dir : _) <- getArgs
  Elm.generateElmModuleWith opts namespaces elm_imports dir defs api_proxy
  where
    opts = Elm.defElmOptions
    namespaces = ["Bridge"]
    defs =
      [ Elm.DefineElm (Proxy :: Proxy BMealPlan),
        Elm.DefineElm (Proxy :: Proxy BRecipeSummary)
      ]
    api_proxy = Proxy :: Proxy GetMealPlans
    elm_imports = Elm.defElmImports
