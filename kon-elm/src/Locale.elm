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
import Calendar exposing (MonthAnchor)
import MealPhase exposing (MealPhase(..))

{- | Locale symbols.
-}
type Locale = LEnUS
            | LJaJP

{- | Locale-dependent functions.
-}
type alias LocaleImpl msg =
    { -- | Format date with Year, Month, day of month and day of week.
      viewDateYMDA : Date -> List (Html msg)
      -- | Format date with day of month and day of week.
    , viewDateDA : Date -> List (Html msg)
      -- | Format date with Month and day of month
    , viewDateMD : Date -> List (Html msg)
    , viewIngredient : BIngredient -> List (Html msg)
      -- | Format a MonthAnchor (year and month)
    , showDateYMDA : Date -> String
    , showMonthAnchor : MonthAnchor -> String
    , showWeekday : Weekday -> String
    , showMealPhase : MealPhase -> String
    , showCalDay : String
    , showIngredients : String
    , showRecipeSteps : String
    , showRecipeReference : String
    , showNavMenuCalList : String
    , showNavMenuCalTable : String
    }

get : Locale -> LocaleImpl msg
get l =
    case l of
        LEnUS -> localeEnUS
        LJaJP -> localeJaJP

localeJaJP : LocaleImpl msg
localeJaJP =
    let result =
            { viewDateYMDA = jaViewDateLong
            , viewDateDA = jaViewDateDA
            , viewDateMD = jaViewDateMD
            , viewIngredient = jaViewIngredient
            , showDateYMDA = jaShowDateYMDA
            , showMonthAnchor = jaShowMonthAnchor
            , showWeekday = jaFormatWeekday
            , showMealPhase = jaShowMealPhase
            , showCalDay = "日付"
            , showIngredients = "材料"
            , showRecipeSteps = "手順"
            , showRecipeReference = "参考"
            , showNavMenuCalList = "リスト"
            , showNavMenuCalTable = "表"
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
                         ( jaViewDateMD date
                           ++ [text " "]
                           ++ (jaWeekdaySpan <| Date.weekday date)
                         )
                     ]
            ]
        year = (String.fromInt <| Date.year date) ++ "年"
    in result

jaViewDateDA : Date -> List (Html msg)
jaViewDateDA d = 
    let result = [text (mday ++ " ")] ++ (jaWeekdaySpan <| Date.weekday d)
        mday = String.fromInt <| Date.day d
    in result

jaViewDateMD : Date -> List (Html msg)
jaViewDateMD d =
    let result = [ text (month ++ "/" ++ day) ]
        month = String.fromInt <| monthToNumber <| Date.month d
        day = String.fromInt <| Date.day d
    in result
    
jaShowMonthAnchor : MonthAnchor -> String
jaShowMonthAnchor ma =
    let result = year ++ "年 " ++ month ++ "月"
        year = String.fromInt ma.year
        month = String.fromInt <| monthToNumber <| ma.month
    in result

jaWeekdaySpan : Weekday -> List (Html msg)
jaWeekdaySpan wday =
    [text "("]
    ++ (spanWeekday wday <| jaFormatWeekday wday)
    ++ [text ")"]

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

jaShowDateYMDA : Date -> String
jaShowDateYMDA d =
    let result = year ++ "年" ++ month ++ "月" ++ day ++ "日 (" ++ wday ++ ")"
        year = String.fromInt <| Date.year d
        month = String.fromInt <| Date.monthToNumber <| Date.month d
        day = String.fromInt <| Date.day d
        wday = jaFormatWeekday <| Date.weekday d
    in result

---------------

localeEnUS : LocaleImpl msg
localeEnUS =
    let result =
            { viewDateYMDA = enViewDateLong
            , viewDateDA = enViewDateDA
            , viewDateMD = enViewDateMD
            , viewIngredient = enViewIngredient
            , showDateYMDA = enShowDateYMDA
            , showMonthAnchor = enShowMonthAnchor
            , showWeekday = enFormatWeekday
            , showMealPhase = enShowMealPhase
            , showCalDay = "Date"
            , showIngredients = "Ingredients"
            , showRecipeSteps = "Steps"
            , showRecipeReference = "Reference"
            , showNavMenuCalList = "List"
            , showNavMenuCalTable = "Table"
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
                           <| ( (enWeekdaySpan <| Date.weekday date)
                                ++ [text ", "]
                                ++ enViewDateMD date
                              )
                     , text ", "
                     , Html.span [Attr.class "clock-year", Attr.class "text-nowrap"]
                         [text <| String.fromInt <| Date.year date]
                     ]
            ]
    in result

enViewDateDA : Date -> List (Html msg)
enViewDateDA d =
    let result = (enWeekdaySpan <| Date.weekday d) ++ [text (", " ++ mday)]
        mday = String.fromInt <| Date.day d
    in result

enViewDateMD : Date -> List (Html msg)
enViewDateMD d = 
    let result = [text (day ++ " " ++ month)]
        day = String.fromInt <| Date.day d
        month = enFormatMonth <| Date.month d
    in result

enWeekdaySpan : Weekday -> List (Html msg)
enWeekdaySpan w = spanWeekday w <| enFormatWeekday w

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

enFormatMonthLong : Month -> String
enFormatMonthLong m =
    case m of
        Jan -> "January"
        Feb -> "February"
        Mar -> "March"
        Apr -> "April"
        May -> "May"
        Jun -> "June"
        Jul -> "July"
        Aug -> "August"
        Sep -> "September"
        Oct -> "October"
        Nov -> "November"
        Dec -> "December"

enShowMonthAnchor : MonthAnchor -> String
enShowMonthAnchor ma =
    let result = month ++ ", " ++ year
        month = enFormatMonthLong ma.month
        year = String.fromInt ma.year
    in result

enShowDateYMDA : Date -> String
enShowDateYMDA d =
    let result = wday ++ " " ++ day ++ " " ++ month ++ ", " ++ year
        wday = enFormatWeekday <| Date.weekday d
        day = String.fromInt <| Date.day d
        month = enFormatMonthLong <| Date.month d
        year = String.fromInt <| Date.year d
    in result
