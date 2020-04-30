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
import String
import Task

import Bridge exposing (BRecipeSummary)
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

{-| Calendar entry
-}
type alias CalEntry =
    { day : Date
    , phase : MealPhase
    , recipeSummary : Maybe BRecipeSummary
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

appInit : () -> Url -> Nav.Key -> (Model, Cmd Msg)
appInit _ _ _ = ( { curTime = Time.millisToPosix 0
                  , timeZone = Time.utc
                  , calendar =
                        [ { day = Date.fromCalendarDate 2020 Time.Apr 29
                          , phase = Lunch
                          , recipeSummary = Just {id = "foo", name = "ほげほげ"}
                        }
                        ]
                  }  ---- This is just an example
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
        InitTime t z -> ({ model | curTime = t, timeZone = z }, Cmd.none)
        TickTime t -> ({ model | curTime = t }, Cmd.none)

appSub : Model -> Sub Msg
appSub _ = Time.every 5000 TickTime

appOnUrlRequest : UrlRequest -> Msg
appOnUrlRequest _ = NoOp

appOnUrlChange : Url -> Msg
appOnUrlChange _ = NoOp

initTime : Cmd Msg
initTime =
    Task.perform identity <| Task.map2 InitTime Time.now Time.here

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

