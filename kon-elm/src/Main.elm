module Main exposing
    (..)

{-| The application main. -}

import Browser
import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Html exposing (Html, div, text, ul, li)
import Time
import Date exposing (Date)
import Date
import Url exposing (Url)
import Result
import String
import Task

import Bridge exposing (BRecipeSummary, BMealPlan)
import Bridge
import CalEntry exposing (CalEntry)
import CalEntry
import MealPhase exposing (MealPhase(..))
import MealPhase

---- Types

{-| The model.
-}
type alias Model =
    { curTime : Time.Posix
    , timeZone : Time.Zone
    , calendar : List CalEntry
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
         | BackendError String

calendarPeriodDays : Int
calendarPeriodDays = 9
        
appInit : () -> Url -> Nav.Key -> (Model, Cmd Msg)
appInit _ _ _ = ( { curTime = Time.millisToPosix 0
                  , timeZone = Time.utc
                  , calendar = []
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
        MealPlansLoaded mps -> (model, Cmd.none) -- TODO
        BackendError _ -> (model, Cmd.none) -- TODO

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
                Err _ -> BackendError "Error in loadMealPlans" -- TODO: encode the HTTP error
    in Bridge.getMealplans (Date.toIsoString start_day) (Date.toIsoString end_day) handle

---- View

viewBody : Model -> List (Html Msg)
viewBody model =
    [ div [] [viewCurTime model.timeZone model.curTime]
    ]
    ++ (List.concat <| List.map viewCalendar model.calendar)

viewCurTime : Time.Zone -> Time.Posix -> Html Msg
viewCurTime zone time =
    let hour = String.padLeft 2 '0' <| String.fromInt <| Time.toHour zone time
        minute = String.padLeft 2 '0' <| String.fromInt <| Time.toMinute zone time
    in text (hour ++ ":" ++ minute)

viewCalendar : CalEntry -> List (Html Msg)
viewCalendar centry =
    let divbody =
            [ ul [] fieldlist
            ]
        fieldlist =
            [ li [] [text ("day: " ++ Date.toIsoString centry.day)]
            , li [] [text ("phase: " ++ MealPhase.toString centry.phase)]
            ]
            ++ case centry.recipeSummary of
                   Nothing -> []
                   Just rs -> [li [] [text ("meal: " ++ rs.name)]]
    in [div [] divbody]

