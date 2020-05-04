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

---- Types

{- | The model.
-}
type alias Model =
    { clock : MClock
    , calendar : MCalendar
    , errorMsg : Maybe String
    }

{- | Clock in the model.
-}
type alias MClock =
    { loaded : Bool
    , curTime : Time.Posix
    , timeZone : Time.Zone
    }

{- | Calendar in the model.
-}
type alias MCalendar =
    { loaded : Bool
    , calendar : Calendar
    }

setClock : Time.Zone -> Time.Posix -> Model -> Model
setClock tz t model = let new_clock = { loaded = True, curTime = t, timeZone = tz }
                      in { model | clock = new_clock }

tickClock : Time.Posix -> Model -> Model
tickClock t model = 
    let c = model.clock
    in { model | clock = { c | curTime = t } }

initCalendar : Time.Zone -> Time.Posix -> Model -> Model
initCalendar tz t model =
    let cals = CalEntry.forDays (Date.fromPosix tz t) calendarPeriodDays
    in { model | calendar = {loaded = False, calendar = cals} }

setCalendar : Calendar -> Model -> Model
setCalendar cal model = { model | calendar = {loaded = True, calendar = cal} }

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
         -- | Initiallize the current time
         | InitTime Time.Posix Time.Zone
         -- | Update the current time
         | TickTime Time.Posix
         | MealPlansLoaded (List BMealPlan)
         | ErrorMsg String

calendarPeriodDays : Int
calendarPeriodDays = 9
        
appInit : () -> Url -> Nav.Key -> (Model, Cmd Msg)
appInit _ _ _ = ( { clock = { loaded = False
                            , curTime = Time.millisToPosix 0
                            , timeZone = Time.utc
                            }
                  , calendar = { loaded = False
                               , calendar = []
                               }
                  , errorMsg = Nothing
                  }
                , initTime
                )

appView : Model -> Document Msg
appView m = { title = "kon-board"
            , body = viewBody m
            }

appUpdate : Msg -> Model -> (Model, Cmd Msg)
appUpdate msg model =
    case msg of
        NoOp -> (model, Cmd.none)
        InitTime t z -> ( initCalendar z t <| setClock z t model
                        , loadMealPlans t z
                        )
        TickTime t -> (tickClock t model, Cmd.none)
        MealPlansLoaded mps ->
            case CalEntry.addMealPlans mps model.calendar.calendar of
                Err e -> ({ model | errorMsg = Just e }, Cmd.none)
                Ok new_cal -> (setCalendar new_cal model, Cmd.none)
        ErrorMsg e -> ({ model | errorMsg = Just e }, Cmd.none)

appSub : Model -> Sub Msg
appSub _ = Time.every 5000 TickTime

appOnUrlRequest : UrlRequest -> Msg
appOnUrlRequest _ = NoOp

appOnUrlChange : Url -> Msg
appOnUrlChange _ = NoOp

initTime : Cmd Msg
initTime =
    Task.perform identity <| Task.map2 InitTime Time.now Time.here

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
        mkClock c = [ div [] [viewCurTime c.timeZone c.curTime]
                    ]
        err_msg =
            case model.errorMsg of
                Nothing -> []
                Just e -> [ div [] [text ("error: " ++ e)]
                          ]
        mkCalendar c = List.concat <| List.map viewCalEntry c.calendar
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

