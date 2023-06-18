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
    , updateReactPRecipeSearch
    , updateAutoPRecipeSearch
    , viewRecipeSearch
    )

import Browser.Navigation as Nav
import Date exposing (Date)
import List
import Url exposing (Url)
import Url.Parser exposing (Parser, oneOf, (</>), (<?>))
import Url.Parser as P
import Url.Parser.Query as PQ
import Url.Builder as B
import Html exposing (Html)
import Html.Events as Events
import Html.Attributes as Attr
import Http
import FeatherIcons as FIcons
import String

import Bridge exposing (BRecipeId, BRecipeStored, BAnswerRecipe, getApiV1Recipes)
import Calendar exposing (MonthAnchor, CalEntry)
import Coming exposing (Coming(..), isPending)
import UpdateM exposing (UpdateM)
import Locale exposing (Locale)
import Locale
import HttpUtil exposing (showHttpError)

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
     | RSRecipeListLoaded (Result Http.Error BAnswerRecipe)

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
                           , pageSize = 5 -- TODO: make this big enough
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
        PageRecipeSearch r -> isPending r.answer

updateReactPRecipeSearch : Nav.Key -> MsgRecipeSearch -> UpdateM PRecipeSearchModel MsgRecipeSearch
updateReactPRecipeSearch key msg model =
    case msg of
        RSUpdateFormQuery q -> ({ model | formQuery = q }, [])
        RSSubmitQuery ->
            let result = (newModel, [Nav.pushUrl key queryUrl])
                queryUrl = B.relative [] [B.string "q" model.formQuery]
                newModel = { model | answer = NotStarted, submittedQuery = Nothing, submittedPage = Nothing }
            in result
        RSRecipeListLoaded ret ->
            case ret of
                Ok a -> ({ model | answer = Success a }, [])
                Err e -> ({ model | answer = Failure <| showHttpError e }, [])

updateAutoPRecipeSearch : UpdateM PRecipeSearchModel MsgRecipeSearch
updateAutoPRecipeSearch =
    let result = loadRecipeList
        loadRecipeList model =
            case (model.submittedQuery, model.answer) of
                (Just sQuery, NotStarted) ->
                    let newModel = { model | answer = Pending }
                        offset = (*) model.pageSize <| Maybe.withDefault 0 model.submittedPage
                        cmd = getApiV1Recipes (Just sQuery) (Just model.pageSize) (Just offset) RSRecipeListLoaded
                    in (newModel, [cmd])
                _ -> (model, [])
    in result

viewRecipeSearch : Locale -> PRecipeSearchModel -> List (Html MsgRecipeSearch)
viewRecipeSearch locale m = 
    let result = searchForm ++ searchResult
        searchForm =
          [ Html.div [Attr.class "row"] [Html.div [Attr.class "col", Attr.class "px-0"]
            [ Html.form [Events.onSubmit RSSubmitQuery, Attr.class "form-search-recipes"]
              [ Html.div [Attr.class "form-row"]
                [ Html.div [Attr.class "col-10"]
                  [ Html.input [ Attr.type_ "search", Attr.class "form-control", Attr.id "q", Attr.name "q", Attr.autofocus True
                               , Attr.placeholder <| (.showNavMenuSearchRecipes) <| Locale.get locale
                               , Attr.value m.formQuery
                               , Events.onInput (\s -> RSUpdateFormQuery s)
                               ] []
                  ]
                , Html.div [Attr.class "col"]
                  [ Html.button [Attr.type_ "submit", Attr.class "btn btn-primary"] [FIcons.toHtml [] <| FIcons.withSize 16 <| FIcons.search] ]
                ]
              ]
            ]]
          ]
        searchResult =
          case m.answer of
            -- TODO: show the total number of items with i18n.
            Success a ->
              [ Html.div [Attr.class "row"] [Html.div [Attr.class "col", Attr.class "px-0"]
                [ paginationForAnswer m a
                , Html.div [Attr.class "list-group"] <| List.map searchAnswerItem a.recipes
                ]]
              ]
            _ -> [] -- TODO: handle Failure
        searchAnswerItem r =
          Html.a [Attr.href <| recipePageLink r.id, Attr.class "list-group-item", Attr.class "list-group-item-action", Attr.class "text-primary"] [Html.text r.name]
    in result

-- TODO: skip some page items when the totalPageNum is too big.
paginationForAnswer : PRecipeSearchModel -> BAnswerRecipe -> Html msg
paginationForAnswer model a =
  let result =
        Html.nav [] [Html.ul [Attr.class "pagination"] (prevItem ++ numbers ++ nextItem)]
      totalPageNum =
        if a.total_count == 0
        then 1
        else
          if remainderBy model.pageSize a.total_count == 0
          then a.total_count // model.pageSize
          else a.total_count // model.pageSize + 1
      curPage = a.offset // model.pageSize
      urlForPage p = B.relative [] <| paramQ ++ [B.int "page" p]
      paramQ = case model.submittedQuery of
                 Nothing -> []
                 Just q -> [B.string "q" q]
      item mp t =
        let (attrsA, attrsLi) =
              case mp of
                Nothing -> ([], [Attr.class "disabled"])
                Just p ->
                  let active = if p == curPage
                               then [Attr.class "active"]
                               else []
                  in ([Attr.href <| urlForPage p], active)
        in Html.li ([Attr.class "page-item"] ++ attrsLi) [Html.a ([Attr.class "page-link"] ++ attrsA) [Html.text t]]
      prevItem =
        if totalPageNum == 0
        then []
        else if curPage <= 0
             then [item Nothing "<<"]
             else [item (Just <| curPage - 1) "<<"]
      nextItem =
        if totalPageNum == 0
        then []
        else if curPage >= (totalPageNum - 1)
             then [item Nothing ">>"]
             else [item (Just <| curPage + 1) ">>"]
      numbers = List.map (\i -> item (Just i) <| String.fromInt (i + 1)) (List.range 0 (totalPageNum - 1)) -- TODO: what if totalPageNum is too large?
  in result
