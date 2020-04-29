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

import Bridge exposing (BRecipeSummary)

---- Types

{-| The model.
-}
type alias Model =
    { curTime : Time.Posix
    , timeZone : Time.Zone
    , calendar : List CalEntry
    }

{-| Phase of a meal.
-}
type MealPhase = Breakfast
               | Lunch
               | Dinner
               | MealOther String

mealPhaseString : MealPhase -> String
mealPhaseString mp =
    case mp of
        Breakfast -> "朝"
        Lunch -> "昼"
        Dinner -> "夜"
        MealOther s -> s

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

type alias Msg = ()

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
                , Cmd.none   
                )

appView : Model -> Document Msg
appView m = { title = "kon-board"
            , body = viewBody m
            }

appUpdate : Msg -> Model -> (Model, Cmd Msg)
appUpdate _ cur_model = (cur_model, Cmd.none)

appSub : Model -> Sub Msg
appSub _ = Sub.none

appOnUrlRequest : UrlRequest -> Msg
appOnUrlRequest _ = ()

appOnUrlChange : Url -> Msg
appOnUrlChange _ = ()

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
            , li [] [text ("phase: " ++ mealPhaseString centry.phase)]
            ]
            ++ case centry.recipeSummary of
                   Nothing -> []
                   Just rs -> [li [] [text ("meal: " ++ rs.name)]]
    in [div [] divbody]

