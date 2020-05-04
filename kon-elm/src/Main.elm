module Main exposing
   (..)

{- | The application main. -}

import Browser
import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Html exposing (Html, div, text, ul, li)
import Http
import List
import Time
import Date exposing (Date)
import Date
import Url exposing (Url)
import Result
import String
import Task

import Bridge exposing (BRecipeSummary, BMealPlan)
import Bridge
import CalEntry exposing (CalEntry, Calendar, DayMeal)
import CalEntry
import MealPhase exposing (MealPhase(..))
import MealPhase

---- Model Types

{- | The model.
-}
type alias Model =
    { -- | 'Nothing' before initialized.
      clock : Maybe MClock
    , calendar : Calendar
      -- | 'True' if MealPlans are loaded to the calendar.
    , mealPlansLoaded : Bool
    , errorMsg : Maybe String
    }

{- | Clock in the model.
-}
type alias MClock =
    { curTime : Time.Posix
    , timeZone : Time.Zone
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

calendarPeriodDays : Int
calendarPeriodDays = 9
        
appInit : () -> Url -> Nav.Key -> (Model, Cmd Msg)
appInit _ _ _ =
    let model = { clock = Nothing
                , calendar = []
                , mealPlansLoaded = False
                , errorMsg = Nothing
                }
    in (model, Cmd.batch <| appUpdateCmd InitModel model)

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

appUpdateCmd : Msg -> Model -> List (Cmd Msg)
appUpdateCmd _ model =
    let result = initTimeCmd ++ loadMealPlanCmd
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
    in result

appSub : Model -> Sub Msg
appSub _ = Time.every 5000 TickTime

appOnUrlRequest : UrlRequest -> Msg
appOnUrlRequest _ = NoOp

appOnUrlChange : Url -> Msg
appOnUrlChange _ = NoOp

loadMealPlans : Time.Posix -> Time.Zone -> Cmd Msg
loadMealPlans time zone =
    let start_day = Date.fromPosix zone time
        end_day = Date.add Date.Days calendarPeriodDays start_day
        handle ret =
            case ret of
                Ok mps -> MealPlansLoaded mps
                Err http_err -> ErrorMsg ("Error in loadMealPlans: " ++ showHttpError http_err)
    in Bridge.getApiV1Mealplans (Date.toIsoString start_day) (Date.toIsoString end_day) handle

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
    let result = (mkClock model.clock) ++ err_msg ++ (mkCalendar model.calendar)
        mkClock mc =
            case mc of
                Nothing -> []
                Just c -> [ div [] [viewCurTime c.timeZone c.curTime] ]
        err_msg =
            case model.errorMsg of
                Nothing -> []
                Just e -> [ div [] [text ("error: " ++ e)]
                          ]
        mkCalendar cal = List.concat <| List.map viewCalEntry cal
    in result

viewCurTime : Time.Zone -> Time.Posix -> Html Msg
viewCurTime zone time =
    let hour = String.padLeft 2 '0' <| String.fromInt <| Time.toHour zone time
        minute = String.padLeft 2 '0' <| String.fromInt <| Time.toMinute zone time
    in text (hour ++ ":" ++ minute)

viewDayMeal : DayMeal -> List (Html Msg)
viewDayMeal dm =
    let result = 
            [ li [] [text ("phase: " ++ MealPhase.toString dm.phase)]
            ]
            ++ List.map mkRecipe dm.recipes
        mkRecipe r = li [] [text ("meal: " ++ r.name)]
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

