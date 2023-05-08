module Page exposing
    ( Page(..)
    , PTopModel
    , PRecipeModel
    , PDayModel
    , PRecipeSearchModel
    , MsgRecipeSearch(..)
    , initPage
    , parseUrl
    , recipePageLink
    , dayPageLink
    , isLoading
    , updatePRecipeSearchModel
    )

import Date exposing (Date)
import Url exposing (Url)
import Url.Parser exposing (Parser, oneOf, (</>))
import Url.Parser as P
import Url.Builder as B

import Bridge exposing (BRecipeId, BRecipeStored)
import Calendar exposing (MonthAnchor, CalEntry)
import Coming exposing (Coming(..), isPending)

{- | The page associated with URL.
-}
type Page =
      -- | The top page
      PageTop PTopModel
      -- | The recipe page
    | PageRecipe PRecipeModel
      -- | The day page
    | PageDay PDayModel
    | PageRecipeSearch PRecipeSearchModel

type alias PTopModel =
    { viewportAdjusted : Coming String ()
    , currentAnchor : Coming String MonthAnchor
    }

type alias PRecipeModel =
    { recipeID : BRecipeId
    , recipe : Coming String BRecipeStored
    }

type alias PDayModel =
    { day : Date
    , calEntry : Coming String CalEntry
    }

type alias PRecipeSearchModel =
     { formQuery : String
     }

type MsgRecipeSearch =
     RSUpdateFormQuery String

initPage : Page
initPage = PageTop { viewportAdjusted = NotStarted, currentAnchor = NotStarted }

initRecipePage : BRecipeId -> Page
initRecipePage rid =
    PageRecipe { recipeID = rid, recipe = NotStarted }

initDayPage : Date -> Page
initDayPage d =
    PageDay { day = d, calEntry = NotStarted }

parseUrl : Url -> Maybe Page
parseUrl = P.parse parserPage

parserDate : Parser (Date -> a) a
parserDate =
    let result = P.custom "DATE" parser
        parser s =
            case Date.fromIsoString s of
                Err _ -> Nothing
                Ok d -> Just d
    in result

parserPage : Parser (Page -> a) a
parserPage =
    oneOf
    [ P.map initPage P.top
    , P.map initRecipePage (P.s "recipes" </> P.string)
    , P.map (PageRecipeSearch { formQuery = "" }) (P.s "recipes" </> P.top)
    , P.map initDayPage (P.s "days" </> parserDate)
    ]

recipePageLink : BRecipeId -> String
recipePageLink rid = B.absolute ["recipes", rid] []

dayPageLink : Date -> String
dayPageLink d = B.absolute ["days", Date.toIsoString d] []

isLoading : Page -> Bool
isLoading p =
    case p of
        PageTop t -> isPending t.viewportAdjusted || isPending t.currentAnchor
        PageRecipe r -> isPending r.recipe
        PageDay d -> isPending d.calEntry
        PageRecipeSearch _ -> False

updatePRecipeSearchModel : MsgRecipeSearch -> PRecipeSearchModel -> PRecipeSearchModel
updatePRecipeSearchModel msg model =
    case msg of
        RSUpdateFormQuery q -> { model | formQuery = q }
