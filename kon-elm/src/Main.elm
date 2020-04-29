module Main exposing
    (..)

{-| The application main. -}

import Browser
import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Html exposing (Html, div, text)
import Time
import Date exposing (Date)
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
                  , calendar = []
                  }
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
viewCalendar _ = [text "TODO: CalEntry"]

