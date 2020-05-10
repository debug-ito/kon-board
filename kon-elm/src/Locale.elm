module Locale exposing
    ( Locale(..)
    , LocaleImpl
    , get
    )

{- | Locale support -}

import Attr
import Date exposing (Date, monthToNumber)
import Html exposing (Html, text)
import Html
import String
import Time exposing (Month)

import MealPhase exposing (MealPhase(..))

{- | Locale symbols.
-}
type Locale = LJaJP

{- | Locale-dependent functions.
-}
type alias LocaleImpl msg =
    { viewDateLong : Date -> [Html msg]
    , viewDateShort : Date -> [Html msg]
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
            , showMealPhase = jaShowMealPhase
            , showCalDay = "日付"
            , showIngredients = "材料"
            , showRecipeSteps = "手順"
            , showRecipeReference = "参考"
            }
        jaShowMealPhase mp =
            case mp of
                Breakfast -> "朝食"
                Lunch -> "昼食"
                Dinner -> "夕食"
                MealOther s -> s
    in result

jaViewDateLong : Date -> [Html msg]
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

jaViewDateShort : Date -> [Html msg]
jaViewDateShort _ = Debug.todo "todo"

jaFormatDay : Date -> String
jaFormatDay d =
    let result = month ++ "/" ++ day ++ " " ++ weekday
        month = String.fromInt <| monthToNumber d
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
