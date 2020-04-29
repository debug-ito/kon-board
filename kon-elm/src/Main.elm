module Main exposing
    (..)

{-| The application main. -}

import Browser
import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Time
import Date exposing (Date)
import Url exposing (Url)

import Bridge exposing (BRecipeSummary)

{-| The model.
-}
type alias Model =
    { curTime : Time.Posix
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
                  , calendar = []
                  }
                , Cmd.none   
                )

appView : Model -> Document Msg
appView _ = { title = "kon-board"
            , body = []
            }

appUpdate : Msg -> Model -> (Model, Cmd Msg)
appUpdate _ cur_model = (cur_model, Cmd.none)

appSub : Model -> Sub Msg
appSub _ = Sub.none

appOnUrlRequest : UrlRequest -> Msg
appOnUrlRequest _ = ()

appOnUrlChange : Url -> Msg
appOnUrlChange _ = ()
