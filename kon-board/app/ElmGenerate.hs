module Main
  ( main
  ) where

import Data.Proxy (Proxy(..))
import qualified Servant.Elm as Elm
-- import qualified Elm.TyRep as ElmT
import System.Environment (getArgs)

import KonBoard.Bridge.MealPlan (BMealPlan)
import KonBoard.Bridge.Recipe (BRecipeSummary)
import KonBoard.Bridge.Time (BDay)
import KonBoard.Web.API (GetMealPlans)

main :: IO ()
main = do
  (dir : _) <- getArgs
  Elm.generateElmModuleWith opts namespaces elm_imports dir defs api_proxy
  where
    opts = Elm.defElmOptions { Elm.elmTypeAlterations = typeAlt
                             }
    namespaces = ["Bridge"]
    defs =
      [ Elm.DefineElm (Proxy :: Proxy BMealPlan),
        Elm.DefineElm (Proxy :: Proxy BRecipeSummary),
        Elm.DefineElm (Proxy :: Proxy BDay)
      ]
    api_proxy = Proxy :: Proxy GetMealPlans
    elm_imports = Elm.defElmImports
    typeAlt = Elm.defaultTypeAlterations
    -- typeAlt (Elm.ETyCon (ElmT.ETCon "Day")) = Elm.ETyCon (ElmT.ETCon "SDay")
    -- typeAlt t = 
