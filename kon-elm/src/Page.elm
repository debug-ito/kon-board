module Page exposing
    ( Page(..)
    , PTopModel
    , PRecipeModel
    , PDayModel
    , initPage
    , parseUrl
    , recipePageLink
    , dayPageLink
    , isLoading
    
      -- * RecipeSearch page
    , PRecipeSearchModel
    , MsgRecipeSearch(..)
    , updatePRecipeSearchModel
    , viewRecipeSearch
    )

import Browser.Navigation as Nav
import Date exposing (Date)
import Url exposing (Url)
import Url.Parser exposing (Parser, oneOf, (</>), (<?>))
import Url.Parser as P
import Url.Parser.Query as PQ
import Url.Builder as B
import Html exposing (Html)
import Html.Events as Events
import Html.Attributes as Attr
import FeatherIcons as FIcons

import Bridge exposing (BRecipeId, BRecipeStored, BAnswerRecipe)
import Calendar exposing (MonthAnchor, CalEntry)
import Coming exposing (Coming(..), isPending)
import UpdateM exposing (UpdateM)
import Locale exposing (Locale)

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
     , submittedQuery : Maybe String
     , submittedPage : Maybe Int
     , answer : Coming String BAnswerRecipe
     , pageSize : Int
     }

type MsgRecipeSearch =
       RSUpdateFormQuery String
     | RSSubmitQuery

initPage : Page
initPage = PageTop { viewportAdjusted = NotStarted, currentAnchor = NotStarted }

initRecipePage : BRecipeId -> Page
initRecipePage rid =
    PageRecipe { recipeID = rid, recipe = NotStarted }

initDayPage : Date -> Page
initDayPage d =
    PageDay { day = d, calEntry = NotStarted }

initRecipeSearchPage : Maybe String -> Maybe Int -> Page
initRecipeSearchPage s p = PageRecipeSearch
                           { formQuery = Maybe.withDefault "" s
                           , submittedQuery = s
                           , submittedPage = p
                           , answer = NotStarted
                           , pageSize = 10
                           }

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
    , P.map initRecipeSearchPage (P.s "recipes" <?> PQ.string "q" <?> PQ.int "page")
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

updatePRecipeSearchModel : Nav.Key -> MsgRecipeSearch -> UpdateM PRecipeSearchModel MsgRecipeSearch
updatePRecipeSearchModel key msg model =
    case msg of
        RSUpdateFormQuery q -> ({ model | formQuery = q }, [])
        RSSubmitQuery ->
            let result = (model, [Nav.pushUrl key queryUrl])
                queryUrl = B.relative [] [B.string "q" model.formQuery]
            in result

viewRecipeSearch : Locale -> PRecipeSearchModel -> List (Html MsgRecipeSearch)
viewRecipeSearch _ m = 
    let result =
          [ Html.form [Events.onSubmit RSSubmitQuery]
            [ Html.div [Attr.class "form-row"]
              [ Html.div [Attr.class "col-10"]
                [ Html.input [ Attr.type_ "search", Attr.class "form-control", Attr.id "q", Attr.name "q", Attr.autofocus True, Attr.placeholder "search recipe"
                             , Attr.value m.formQuery
                             , Events.onInput (\s -> RSUpdateFormQuery s)
                             ] []
                ] -- TODO: i18n
              , Html.div [Attr.class "col"]
                [ Html.button [Attr.type_ "submit", Attr.class "btn btn-primary"] [FIcons.toHtml [] <| FIcons.withSize 16 <| FIcons.search] ]
              ]
            ]
          ]
    in result
                


