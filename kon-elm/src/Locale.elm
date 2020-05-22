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
import Time exposing (Month(..), Weekday(..))

import Bridge exposing (BIngredient)
import MealPhase exposing (MealPhase(..))

{- | Locale symbols.
-}
type Locale = LEnUS
            | LJaJP

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
        LEnUS -> localeEnUS
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
        jaViewIngredient ing = [text (ing.food ++ ": " ++ ing.qtty)]
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
                         <| jaViewDateShort date
                     ]
            ]
        year = (String.fromInt <| Date.year date) ++ "年"
    in result

jaViewDateShort : Date -> List (Html msg)
jaViewDateShort d =
    let result = [ text (month ++ "/" ++ day ++ " ") ] ++ weekday
        month = String.fromInt <| monthToNumber <| Date.month d
        day = String.fromInt <| Date.day d
        wday = Date.weekday d
        weekday = [text "("]
                  ++ spanWeekday wday (jaFormatWeekday wday)
                  ++ [text ")"]
    in result

spanWeekday : Weekday -> String -> List (Html msg)
spanWeekday w t =
    let result = [Html.span [Attr.class weekday_class] [text t]]
        weekday_class =
            case w of
                Sat -> "day-sat"
                Sun -> "day-sun"
                _ -> "day-week"
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

---------------

localeEnUS : LocaleImpl msg
localeEnUS =
    let result =
            { viewDateLong = enViewDateLong
            , viewDateShort = enViewDateShort
            , viewIngredient = enViewIngredient
            , showMealPhase = enShowMealPhase
            , showCalDay = "Date"
            , showIngredients = "Ingredients"
            , showRecipeSteps = "Steps"
            , showRecipeReference = "Reference"
            }
        enViewIngredient ing = [text (ing.qtty ++ ", " ++ ing.food)]
        enShowMealPhase mp =
            case mp of
                Breakfast -> "Breakfast"
                Lunch -> "Lunch"
                Dinner -> "Dinner"
                MealOther s -> s
    in result

enViewDateLong : Date -> List (Html msg)
enViewDateLong date =
    let result =
            [ div [] [ Html.span [Attr.class "clock-day", Attr.class "text-nowrap"]
                           <| enViewDateShort date
                     , text ", "
                     , Html.span [Attr.class "clock-year", Attr.class "text-nowrap"]
                         [text <| String.fromInt <| Date.year date]
                     ]
            ]
    in result

enViewDateShort : Date -> List (Html msg)
enViewDateShort d =
    let result = weekday
                 ++ [text (", " ++ day ++ " " ++ month)]
        weekday = spanWeekday wday <| enFormatWeekday wday
        wday = Date.weekday d
        day = String.fromInt <| Date.day d
        month = enFormatMonth <| Date.month d
    in result

enFormatWeekday : Weekday -> String
enFormatWeekday w =
    case w of
        Mon -> "Mon"
        Tue -> "Tue"
        Wed -> "Wed"
        Thu -> "Thu"
        Fri -> "Fri"
        Sat -> "Sat"
        Sun -> "Sun"

enFormatMonth : Month -> String
enFormatMonth m =
    case m of
        Jan -> "Jan"
        Feb -> "Feb"
        Mar -> "Mar"
        Apr -> "Apr"
        May -> "May"
        Jun -> "Jun"
        Jul -> "Jul"
        Aug -> "Aug"
        Sep -> "Sep"
        Oct -> "Oct"
        Nov -> "Nov"
        Dec -> "Dec"
