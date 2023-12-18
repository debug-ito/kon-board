module Page exposing
    ( Page(..)
    , PTopModel
    , PDayModel
    , initPage
    , parseUrl
    , recipePageLink
    , dayPageLink
    , isLoading
    , replaceHashtags

    -- * Recipe page
    , PRecipeModel
    , MsgRecipe(..)
    , viewRecipePage
    , updateAutoPRecipe
    , updateReactPRecipe
    , errorMsgRecipe
    
      -- * RecipeSearch page
    , PRecipeSearchModel
    , MsgRecipeSearch(..)
    , updateReactPRecipeSearch
    , updateAutoPRecipeSearch
    , errorMsgRecipeSearch
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
import Html
import Html.Events as Events
import Html.Attributes as Attr
import Http
import FeatherIcons as FIcons
import String
import Json.Decode as Decode
import Markdown

import Bridge exposing (BRecipeId, BRecipeStored, BAnswerRecipe, getApiV1Recipes, BIngDesc(..))
import Calendar exposing (MonthAnchor, CalEntry)
import Coming exposing (Coming(..), isPending)
import UpdateM exposing (UpdateM)
import Locale exposing (Locale)
import Locale
import HttpUtil exposing (showHttpError)
import Ports exposing (portSelectElement)

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

type MsgRecipe =
       RecipeLoaded (Result String (BRecipeId, BRecipeStored))
     | RecipeSelectName

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
     | RSSelectElem String

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
                           , pageSize = 50
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

viewRecipePage : Locale -> PRecipeModel -> List (Html MsgRecipe)
viewRecipePage locale rmodel =
    let result = recipe_body
                 ++ [Html.div [Attr.class "recipe-id-box"] [Html.text ("Recipe ID: " ++ rmodel.recipeID)]]
        recipe_body =
            case Coming.success rmodel.recipe of -- TODO: handle Failure case and render error message.
                Nothing -> []
                Just r -> viewRecipe locale r
    in result

viewRecipe : Locale -> BRecipeStored -> List (Html MsgRecipe)
viewRecipe locale br =
    let result = [Html.div [Attr.class "recipe-box"] recipeContent]
        recipeContent = viewName br.name ++ viewIngDescs br.ings ++ viewDesc br.desc ++ viewRefs br.ref
        viewName n =
            [ Html.h1 [] ([Html.text n] ++ [copyButton RecipeSelectName [Attr.class "float-right"]])
            ]
        viewIngDescs ings =
            if List.isEmpty ings
            then []
            else [ Html.h2 [] [Html.text <| (.showIngredients) <| Locale.get locale]
                 , Html.ul [] <| List.concatMap viewIngDesc ings
                 ]
        viewIngDesc ingDesc =
            case ingDesc of
                BIngGroup ingG -> [ Html.li [] [Html.text ingG.g]
                                  , Html.ul [] <| List.map viewIng ingG.ings
                                  ]
                BIngSingle ing -> [viewIng ing]
        viewIng ing = Html.li [] <| (.viewIngredient) (Locale.get locale) ing
        linkHashtags = replaceHashtags (\t -> "[#" ++ t ++ "](" ++ linkUrlForHashtag t ++ ")")
        linkUrlForHashtag tag = B.absolute ["recipes"] [B.string "q" <| "#" ++ tag]
        viewDesc desc =
            if desc == ""
            then []
            else [Html.h2 [] [Html.text <| (.showRecipeSteps) <| Locale.get locale]] ++ Markdown.toHtml Nothing (linkHashtags desc)
        viewRefs refs =
            if List.isEmpty refs
            then []
            else [ Html.h2 [] [Html.text <| (.showRecipeReference) <| Locale.get locale]
                 , Html.ul [] <| List.concatMap viewRef refs
                 ]
        viewRefLi content = Html.li [Attr.class "recipe-ref"] content
        viewExtLink url t = [Html.a [Attr.href url, Attr.target "_blank"] [Html.text t]]
        viewRef ref =
            case (ref.source, ref.url) of
                (Nothing, Nothing) -> []
                (Just s, Nothing)  -> [viewRefLi <| [Html.text s]]
                (Nothing, Just u)  -> [viewRefLi <| viewExtLink u u]
                (Just s, Just u)   -> [viewRefLi <| viewExtLink u s]
    in result

updateAutoPRecipe : UpdateM PRecipeModel MsgRecipe
updateAutoPRecipe rp =
    case rp.recipe of
        NotStarted -> ({ rp | recipe = Pending }, [loadRecipeByID rp.recipeID])
        _ -> (rp, [])

errorMsgRecipe : MsgRecipe -> Maybe String
errorMsgRecipe m =
    case m of
        RecipeLoaded (Err e) -> Just e
        _ -> Nothing

loadRecipeByID : BRecipeId -> Cmd MsgRecipe
loadRecipeByID rid =
    let result = Bridge.getApiV1RecipesByRecipeid rid handle
        handle ret =
            let content =
                    case ret of
                        Ok r -> Ok (rid, r)
                        Err err -> Err <| "Error in loadRecipeByID: " ++ showHttpError err
            in RecipeLoaded content
    in result

updateReactPRecipe : MsgRecipe -> UpdateM PRecipeModel MsgRecipe
updateReactPRecipe msg rp =
    case msg of
        RecipeLoaded ret ->
            let setError e = { rp | recipe = Failure e }
                resultModel =
                    case ret of
                        Err e -> setError e
                        Ok (rid, r) ->
                                if rp.recipeID == rid
                                then { rp | recipe = Success r }
                                else setError ("Got recipe for " ++ rid ++ ", but expects " ++ rp.recipeID)
            in (resultModel, [])
        RecipeSelectName -> (rp, []) -- TODO

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
        RSSelectElem elemId -> (model, [portSelectElement elemId])

errorMsgRecipeSearch : MsgRecipeSearch -> Maybe String
errorMsgRecipeSearch m =
  case m of
     RSRecipeListLoaded (Err e) -> Just <| showHttpError e
     _ -> Nothing

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
            Success a -> searchResultContainer
                           <| searchResultTotalNum a
                              ++ paginationForAnswer m a
                              ++ [Html.div [Attr.class "list-group"] <| List.map2 searchAnswerItem (List.range 0 (List.length a.recipes - 1)) a.recipes]
                              ++ paginationForAnswer m a
            _ -> []
            -- On Failure, error message is shown by Main.
        searchAnswerItem index r =
          let itemId = "search-answer-item-" ++ String.fromInt index
          in Html.a
             [Attr.href <| recipePageLink r.id, Attr.class "list-group-item", Attr.class "list-group-item-action", Attr.class "text-primary"]
             [Html.span [Attr.id itemId] [Html.text r.name], searchAnswerItemCopyButton itemId]
        searchAnswerItemCopyButton itemId = copyButton (RSSelectElem itemId) [Attr.class "float-right"]
        searchResultContainer e =
          [ Html.div [Attr.class "row"] [Html.div [Attr.class "col", Attr.class "px-0"] e] ]
        searchResultTotalNum a =
          [ Html.div [] [Html.text <| (.showTotalNumOfRecipes) (Locale.get locale) a.total_count] ]
    in result

copyButton : msg -> List (Html.Attribute msg) -> Html msg
copyButton m extraAttrs =
    let attrs = [ Attr.type_ "button", Attr.class "btn", Attr.class "btn-outline-secondary", Attr.class "btn-sm"
                , Events.custom "click" <| Decode.succeed { message = m, stopPropagation  = True, preventDefault = True }
                ]
                ++ extraAttrs
    in Html.button attrs [FIcons.toHtml [] <| FIcons.withSize 16 <| FIcons.clipboard]

-- TODO: skip some page items when the totalPageNum is too big.
paginationForAnswer : PRecipeSearchModel -> BAnswerRecipe -> List (Html msg)
paginationForAnswer model a =
  let result =
        if totalPageNum <= 1
        then []
        else [Html.nav [] [Html.ul [Attr.class "pagination"] (prevItem ++ numbers ++ nextItem)]]
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
      numbers = List.map (\i -> item (Just i) <| String.fromInt (i + 1)) (List.range 0 (totalPageNum - 1))
  in result

type FoldContext = InBlank | InNonBlank | InHashtag String

type alias FoldState =
     { result : String
     , context : FoldContext
     }

type CharClass = CCBlank | CCHashtagStart | CCOther

-- | Replace all occurrences of hashtags in the input text with the string got by the given transformation.
replaceHashtags : (String -> String) -- ^ the input is the hashtag value (without the "#" symbol), the output should be the text to be put in the result.
                -> String -- ^ the input text
                -> String
replaceHashtags transformHashtag input =
  let result = finalize <| String.foldl f { result = "", context = InBlank } input
      f c inState =
          case (inState.context, charClass c) of
              (InBlank, CCBlank) -> readChar c InBlank inState
              (InBlank, CCHashtagStart) -> { inState | context = InHashtag ""}
              (InBlank, CCOther) -> readChar c InNonBlank inState
              (InNonBlank, CCBlank) -> readChar c InBlank inState
              (InNonBlank, _) -> readChar c InNonBlank inState
              (InHashtag t, CCBlank) -> readChar c InBlank <| flushInHashtag inState
              (InHashtag t, CCHashtagStart) -> readChar c InNonBlank <| { inState | result = inState.result ++ "#" ++ t }
              (InHashtag t, CCOther) -> { inState | context = InHashtag <| t ++ String.fromChar c }
      isBlank c = c == ' ' || c == '\t' || c == '\r' || c == '\n' || c == '　'
      isHashtagStart c = c == '#'
      charClass c = if isBlank c
                    then CCBlank
                    else if isHashtagStart c
                      then CCHashtagStart
                      else CCOther
      readChar c ctx state = { result = state.result ++ String.fromChar c, context = ctx }
      flushInHashtag state =
          case state.context of
            InHashtag t -> if t == ""
                           then { state | result = state.result ++ "#" }
                           else { state | result = state.result ++ transformHashtag t }
            _ -> state
      finalize state = (.result) <| flushInHashtag state
  in result

  
