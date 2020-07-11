module Page exposing
    ( Page(..)
    , PTopModel
    , PRecipeModel
    , initPage
    , parseUrl
    , recipePageLink
    , isLoading
    )

import Url exposing (Url)
import Url.Parser exposing (Parser, oneOf, (</>))
import Url.Parser as P
import Url.Builder as B

import Bridge exposing (BRecipeID, BRecipe)
import Calendar exposing (MonthAnchor)
import Coming exposing (Coming(..), isPending)

{- | The page associated with URL.
-}
type Page =
      -- | The top page
      PageTop PTopModel
      -- | The recipe page
    | PageRecipe PRecipeModel

type alias PTopModel =
    { viewportAdjusted : Coming String ()
    , currentAnchor : Coming String MonthAnchor
    }

type alias PRecipeModel =
    { recipeID : BRecipeID
    , recipe : Coming String BRecipe
    }

initPage : Page
initPage = PageTop { viewportAdjusted = NotStarted, currentAnchor = NotStarted }

initRecipePage : BRecipeID -> Page
initRecipePage rid =
    PageRecipe { recipeID = rid, recipe = NotStarted }

parseUrl : Url -> Maybe Page
parseUrl = P.parse parserPage

parserPage : Parser (Page -> a) a
parserPage =
    oneOf
    [ P.map initPage P.top
    , P.map initRecipePage (P.s "recipes" </> P.string)
    ]

recipePageLink : BRecipeID -> String
recipePageLink rid = B.absolute ["recipes", rid] []

isLoading : Page -> Bool
isLoading p =
    case p of
        PageTop t -> isPending t.viewportAdjusted || isPending t.currentAnchor
        PageRecipe r -> isPending r.recipe
