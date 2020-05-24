-- |
-- Module: KonBoard.Exec.GenElm
-- Description: Executable to generate Elm code for kon-board
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module KonBoard.Exec.GenElm
  ( main
  ) where


import Data.Proxy (Proxy(..))
import qualified Servant.Elm as Elm
import qualified Elm.TyRep as ElmT
import System.Environment (getArgs)

import KonBoard.Bridge.MealPlan (BMealPlan)
import KonBoard.Bridge.Recipe
  ( BRecipeSummary, BRecipe, BRecipeIn, BRecipeID, BRecipeExt,
    BRecipeURL, BIngDesc, BIngredient
  )
import KonBoard.Bridge.Time (BDay)
import KonBoard.Web.API (DataAPI)

main :: IO ()
main = do
  (dir : _) <- getArgs
  Elm.generateElmModuleWith opts namespaces elm_imports dir defs api_proxy
  where
    opts = Elm.defElmOptions { Elm.elmTypeAlterations = typeAlt,
                               Elm.elmToString = customToString
                             }
    namespaces = ["Bridge"]
    defs =
      [ Elm.DefineElm (Proxy :: Proxy BMealPlan),
        Elm.DefineElm (Proxy :: Proxy BRecipeSummary),
        Elm.DefineElm (Proxy :: Proxy BRecipeID),
        Elm.DefineElm (Proxy :: Proxy BDay),
        Elm.DefineElm (Proxy :: Proxy BRecipe),
        Elm.DefineElm (Proxy :: Proxy BRecipeIn),
        Elm.DefineElm (Proxy :: Proxy BRecipeExt),
        Elm.DefineElm (Proxy :: Proxy BRecipeURL),
        Elm.DefineElm (Proxy :: Proxy BIngDesc),
        Elm.DefineElm (Proxy :: Proxy BIngredient)
      ]
    api_proxy = Proxy :: Proxy DataAPI
    elm_imports = Elm.defElmImports
    typeAlt = Elm.defaultTypeAlterations
    customToString (Elm.ETyCon (ElmT.ETCon "BDay")) = "identity"
    customToString (Elm.ETyCon (ElmT.ETCon "BRecipeID")) = "identity"
    customToString t = Elm.defaultElmToString t
