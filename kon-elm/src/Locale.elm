module Locale exposing
    ( Locale(..)
    , LocaleImpl
    , get
    )

{- | Locale support -}

import Date exposing (Date, monthToNumber)
import Html exposing (Html, text, div)
import Html
import Html.Attributes as Attr
import String
import Time exposing (Month, Weekday(..))

import Bridge exposing (BIngredient)
import MealPhase exposing (MealPhase(..))

{- | Locale symbols.
-}
type Locale = LJaJP

{- | Locale-dependent functions.
-}
type alias LocaleImpl msg =
    { viewDateLong : Date -> List (Html msg)
    , viewDateShort : Date -> List (Html msg)
    , viewIngredient : BIngredient -> List (Html msg)
    , showMealPhase : MealPhase -> String
    , showCalDay : String
    , showIngredients : String
    , showRecipeSteps : String
    , showRecipeReference : String
    }

get : Locale -> LocaleImpl msg
get l =
    case l of
        LJaJP -> localeJaJP

localeJaJP : LocaleImpl msg
localeJaJP =
    let result =
            { viewDateLong = jaViewDateLong
            , viewDateShort = jaViewDateShort
            , viewIngredient = jaViewIngredient
            , showMealPhase = jaShowMealPhase
            , showCalDay = "日付"
            , showIngredients = "材料"
            , showRecipeSteps = "手順"
            , showRecipeReference = "参考"
            }
        jaViewIngredient ing = [Html.li [] [text (ing.food ++ ": " ++ ing.qtty)]]
        jaShowMealPhase mp =
            case mp of
                Breakfast -> "朝食"
                Lunch -> "昼食"
                Dinner -> "夕食"
                MealOther s -> s
    in result

jaViewDateLong : Date -> List (Html msg)
jaViewDateLong date =
    let result =
            [ div [] [ Html.span [Attr.class "clock-year", Attr.class "text-nowrap"]
                           [text year]
                     , text " "
                     , Html.span [Attr.class "clock-day", Attr.class "text-nowrap"]
                         [text <| jaFormatDay date]
                     ]
            ]
        year = (String.fromInt <| Date.year date) ++ "年"
    in result

jaViewDateShort : Date -> List (Html msg)
jaViewDateShort d = [text <| jaFormatDay d]

jaFormatDay : Date -> String
jaFormatDay d =
    let result = month ++ "/" ++ day ++ " " ++ weekday
        month = String.fromInt <| monthToNumber <| Date.month d
        day = String.fromInt <| Date.day d
        weekday = "(" ++ (jaFormatWeekday <| Date.weekday d) ++ ")"
    in result

jaFormatWeekday : Weekday -> String
jaFormatWeekday w =
    case w of
        Mon -> "月"
        Tue -> "火"
        Wed -> "水"
        Thu -> "木"
        Fri -> "金"
        Sat -> "土"
        Sun -> "日"
