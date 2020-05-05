module Main exposing
   (..)

{- | The application main. -}

import Browser
import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Html exposing (Html, div, text, ul, li, h1, h2)
import Html
import Html.Attributes exposing (href)
import Html.Attributes as Attr
import Http
import List
import Markdown
import Time
import Date exposing (Date)
import Date
import Url exposing (Url)
import Url
import Url.Builder as UrlB
import Result
import String
import Task

import Bridge exposing
    (BRecipeSummary, BMealPlan, BRecipeID, BRecipe(..))
import Bridge
import CalEntry exposing (CalEntry, Calendar, DayMeal)
import CalEntry
import MealPhase exposing (MealPhase(..))
import MealPhase
import Page exposing (Page(..), recipePageLink)
import Page

---- Model Types

{- | The model.
-}
type alias Model =
    { -- | 'Nothing' before initialized.
      clock : Maybe MClock
    , page : Page
    , navKey : Nav.Key
    , calendar : Calendar
      -- | 'True' if MealPlans are loaded to the calendar.
    , mealPlansLoaded : Bool
    , errorMsg : Maybe String
    , loadedRecipe : Maybe MRecipe
    }

{- | Clock in the model.
-}
type alias MClock =
    { curTime : Time.Posix
    , timeZone : Time.Zone
    }

{- | Loaded recipe in the model.
-}
type alias MRecipe =
    { id : BRecipeID
    , recipe : BRecipe
    }

setClock : Time.Zone -> Time.Posix -> Model -> Model
setClock tz t model = { model | clock = Just { curTime = t, timeZone = tz } }

tickClock : Time.Posix -> Model -> Model
tickClock t model =
    case model.clock of
        Nothing -> model
        Just c -> { model | clock = Just { c | curTime = t } }

initCalendar : Time.Zone -> Time.Posix -> Model -> Model
initCalendar tz t model =
    let cals = CalEntry.forDays (Date.fromPosix tz t) calendarPeriodDays
    in { model | calendar = cals }

---- Main

main = Browser.application
       { init = appInit
       , view = appView
       , update = appUpdate
       , subscriptions = appSub
       , onUrlRequest = appOnUrlRequest
       , onUrlChange = appOnUrlChange
       }

{- | Main program msg
-}
type Msg = NoOp
         | InitModel
         -- | Initiallize the current time
         | InitTime Time.Posix Time.Zone
         -- | Update the current time
         | TickTime Time.Posix
         | MealPlansLoaded (List BMealPlan)
         | ErrorMsg String
         | UrlRequestMsg UrlRequest
         | UrlChangeMsg Url
         | RecipeLoaded MRecipe

calendarPeriodDays : Int
calendarPeriodDays = 9
        
appInit : () -> Url -> Nav.Key -> (Model, Cmd Msg)
appInit _ url key =
    let model_base = { clock = Nothing
                     , page = PageTop
                     , navKey = key
                     , calendar = []
                     , mealPlansLoaded = False
                     , errorMsg = Nothing
                     , loadedRecipe = Nothing
                     }
        model = appUrlChange url model_base
        cmd = Cmd.batch <| appUpdateCmd InitModel model
    in (model, cmd)

appView : Model -> Document Msg
appView m = { title = "kon-board"
            , body = viewBody m
            }

appUpdate : Msg -> Model -> (Model, Cmd Msg)
appUpdate msg model =
    let new_model = appUpdateModel msg model
        cmds = appUpdateCmd msg new_model
    in (new_model, Cmd.batch cmds)

appUpdateModel : Msg -> Model -> Model
appUpdateModel msg model =
    case msg of
        NoOp -> model
        InitModel -> model
        InitTime t z -> initCalendar z t <| setClock z t model
        TickTime t -> tickClock t model
        MealPlansLoaded mps ->
            case CalEntry.addMealPlans mps model.calendar of
                Err e -> { model | errorMsg = Just e }
                Ok new_cal -> { model | calendar = new_cal, mealPlansLoaded = True }
        ErrorMsg e -> { model | errorMsg = Just e }
        UrlRequestMsg _ -> model
        UrlChangeMsg u -> appUrlChange u model
        RecipeLoaded mr -> { model | loadedRecipe = Just mr }

appUrlChange : Url -> Model -> Model
appUrlChange u model = 
    case Page.parseUrl u of
        Nothing -> let err = ("Unknown URL: " ++ Url.toString u)
                   in { model | errorMsg = Just err }
        Just p -> { model | page = p, loadedRecipe = Nothing }

appUpdateCmd : Msg -> Model -> List (Cmd Msg)
appUpdateCmd msg model =
    let result = initTimeCmd ++ loadMealPlanCmd ++ urlRequestCmd ++ loadRecipeCmd
        initTimeCmd =
            case model.clock of
                Nothing -> [Task.perform identity <| Task.map2 InitTime Time.now Time.here]
                Just _ -> []
        loadMealPlanCmd =
            if model.mealPlansLoaded
            then []
            else
                case model.clock of
                    Nothing -> []
                    Just c -> [loadMealPlans c.curTime c.timeZone]
        urlRequestCmd =
            case msg of
                UrlRequestMsg (Browser.Internal u) ->
                    [Nav.pushUrl model.navKey <| Url.toString u]
                UrlRequestMsg (Browser.External s) ->
                    [Nav.load s]
                _ -> []
        loadRecipeCmd =
            case (model.page, model.loadedRecipe) of
                (PageRecipe rid, Nothing) -> [loadRecipeByID rid]
                (PageRecipe rid, Just mr) -> if rid == mr.id
                                             then []
                                             else [loadRecipeByID rid]
                _ -> []
    in result

appSub : Model -> Sub Msg
appSub _ = Time.every 5000 TickTime

appOnUrlRequest : UrlRequest -> Msg
appOnUrlRequest = UrlRequestMsg

appOnUrlChange : Url -> Msg
appOnUrlChange = UrlChangeMsg

loadMealPlans : Time.Posix -> Time.Zone -> Cmd Msg
loadMealPlans time zone =
    let start_day = Date.fromPosix zone time
        end_day = Date.add Date.Days calendarPeriodDays start_day
        handle ret =
            case ret of
                Ok mps -> MealPlansLoaded mps
                Err http_err -> ErrorMsg ("Error in loadMealPlans: " ++ showHttpError http_err)
    in Bridge.getApiV1Mealplans (Date.toIsoString start_day) (Date.toIsoString end_day) handle

loadRecipeByID : BRecipeID -> Cmd Msg
loadRecipeByID rid =
    let result = Bridge.getApiV1RecipesByRecipeid rid handle
        handle ret =
            case ret of
                Ok r -> RecipeLoaded { id = rid, recipe = r }
                Err err -> ErrorMsg ("Error in loadRecipeByID: " ++ showHttpError err)
    in result

showHttpError : Http.Error -> String
showHttpError e =
    case e of
        Http.BadUrl u -> "Bad URL: " ++ u
        Http.Timeout -> "HTTP timeout"
        Http.NetworkError -> "Network error"
        Http.BadStatus s -> "Server returned error status " ++ String.fromInt s
        Http.BadBody b -> "Bad HTTP response body: " ++ b

---- View

viewBody : Model -> List (Html Msg)
viewBody model =
    let result = (mkClock model.clock) ++ err_msg ++ viewNav model.page ++ view_for_page
        mkClock mc = viewCurTime mc
        err_msg =
            case model.errorMsg of
                Nothing -> []
                Just e -> [ div [] [text ("error: " ++ e)]
                          ]
        view_for_page =
            case model.page of
                PageTop -> mkCalendar model.calendar
                PageRecipe r -> viewRecipePage r model
        mkCalendar cal = List.concat <| List.map viewCalEntry cal
    in result

viewCurTime : Maybe MClock -> List (Html Msg)
viewCurTime mc =
    case mc of
        Nothing -> []
        Just c ->
            let result = [div [] [text (hour ++ ":" ++ minute)]]
                hour = String.padLeft 2 '0' <| String.fromInt <| Time.toHour c.timeZone c.curTime
                minute = String.padLeft 2 '0' <| String.fromInt <| Time.toMinute c.timeZone c.curTime
            in result

viewNav : Page -> List (Html Msg)
viewNav p =
    case p of
        PageTop -> []
        PageRecipe _ -> [div [] [Html.a [href <| UrlB.absolute [] []] [text "←戻る"]]]
                  
viewDayMeal : DayMeal -> List (Html Msg)
viewDayMeal dm =
    let result = 
            [ li [] [text ("phase: " ++ MealPhase.toString dm.phase)]
            ]
            ++ List.map mkRecipe dm.recipes
        mkRecipe r = li [] <| viewLinkRecipe r.id [text ("meal: " ++ r.name)]
    in result

viewCalEntry : CalEntry -> List (Html Msg)
viewCalEntry centry =
    let result = [div [] [ul [] fieldlist]]
        fieldlist =
            [ li [] [text ("day: " ++ Date.toIsoString centry.day)]
            ]
            ++ List.map mkMeal centry.meals
        mkMeal ml = ul [] (viewDayMeal ml)
    in result

viewLinkRecipe : BRecipeID -> List (Html a) -> List (Html a)
viewLinkRecipe rid content =
    let result = [Html.a attrs content]
        attrs = [href <| recipePageLink rid]
    in result

viewRecipePage : BRecipeID -> Model -> List (Html Msg)
viewRecipePage rid model =
    let result = [div [] [text ("Recipe ID: " ++ rid)]] ++ recipe_body
        recipe_body =
            case model.loadedRecipe of
                Nothing -> []
                Just mr -> viewRecipe mr.recipe
    in result

viewRecipe : BRecipe -> List (Html Msg)
viewRecipe br =
    let result = [div [] recipe_content]
        recipe_content = 
            case br of
                BRIn rin -> viewRecipeIn rin
                BRURL ru -> viewRecipeURL ru
        viewName n = [h1 [] [text n]]
        viewIngDescs ings = [ h2 [] [text "材料"]
                            , ul [] <| List.concatMap viewIngDesc ings
                            ]
        viewIngDesc ing =
            case ing.group of
                Nothing -> List.map viewIng ing.ings
                Just g -> [ li [] [text g]
                          , ul [] <| List.map viewIng ing.ings
                          ]
        viewIng ing = li [] [text (ing.food ++ ": " ++ ing.qtty)]
        viewDesc desc = [h2 [] [text "手順"]] ++ Markdown.toHtml Nothing desc
        viewRefURL ru =
            case ru of
                Nothing -> []
                Just u -> [ h2 [] [text "参考"]
                          , ul [] [li [] [Html.a [href u, Attr.target "_blank"] [text u]]]
                          ]
        viewRecipeIn rin = viewName rin.name ++ viewIngDescs rin.ings
                           ++ viewDesc rin.desc ++ viewRefURL rin.ref_url
        viewRecipeURL ru = viewName ru.name ++ viewRefURL (Just ru.url)
    in result
