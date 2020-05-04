module Main exposing
    (..)

{-| The application main. -}

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

{-| The model.
-}
type alias Model =
    { curTime : Time.Posix
    , timeZone : Time.Zone
    , calendar : Calendar
    , errorMsg : Maybe String
    }
    
---- Main

main = Browser.application
       { init = appInit
       , view = appView
       , update = appUpdate
       , subscriptions = appSub
       , onUrlRequest = appOnUrlRequest
       , onUrlChange = appOnUrlChange
       }

{-| Main program msg
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
appInit _ _ _ = ( { curTime = Time.millisToPosix 0
                  , timeZone = Time.utc
                  , calendar = []
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
        InitTime t z -> ( { model |
                            curTime = t
                          , timeZone = z
                          , calendar = CalEntry.forDays (Date.fromPosix z t) calendarPeriodDays
                          }
                        , loadMealPlans t z
                        )
        TickTime t -> ({ model | curTime = t }, Cmd.none)
        MealPlansLoaded mps ->
            case CalEntry.addMealPlans mps model.calendar of
                Err e -> ({ model | errorMsg = Just e }, Cmd.none)
                Ok new_cals -> ({ model | calendar = new_cals }, Cmd.none)
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
    let result = clock ++ err_msg ++ calendar
        clock = [ div [] [viewCurTime model.timeZone model.curTime]
                ]
        err_msg =
            case model.errorMsg of
                Nothing -> []
                Just e -> [ div [] [text ("error: " ++ e)]
                          ]
        calendar = List.concat <| List.map viewCalEntry model.calendar
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

