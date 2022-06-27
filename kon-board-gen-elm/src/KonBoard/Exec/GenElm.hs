-- | Executable to generate Elm code for kon-board
module KonBoard.Exec.GenElm
    ( main
    ) where


import           Data.Proxy               (Proxy (..))
import qualified Elm.TyRep                as ElmT
import qualified Servant.Elm              as Elm
import           System.Environment       (getArgs)

import           KonBoard.Bridge.MealPlan (BMealPlan)
import           KonBoard.Bridge.Recipe   (BIngDesc, BIngGrouped, BIngredient, BRecipeId,
                                           BRecipeStored, BRef)
import           KonBoard.Bridge.Time     (BDay)
import           KonBoard.Web.API         (DataAPI)

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
      [ Elm.DefineElm (Proxy :: Proxy BMealPlan)
      , Elm.DefineElm (Proxy :: Proxy BRecipeStored)
      , Elm.DefineElm (Proxy :: Proxy BRecipeId)
      , Elm.DefineElm (Proxy :: Proxy BDay)
      , Elm.DefineElm (Proxy :: Proxy BRef)
      , Elm.DefineElm (Proxy :: Proxy BIngDesc)
      , Elm.DefineElm (Proxy :: Proxy BIngredient)
      , Elm.DefineElm (Proxy :: Proxy BIngGrouped)
      ]
    api_proxy = Proxy :: Proxy DataAPI
    elm_imports = Elm.defElmImports
    typeAlt = Elm.defaultTypeAlterations
    customToString (Elm.ETyCon (ElmT.ETCon "BDay"))      = "identity"
    customToString (Elm.ETyCon (ElmT.ETCon "BRecipeId")) = "identity"
    customToString t                                     = Elm.defaultElmToString t
