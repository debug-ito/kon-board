module DateUtil exposing
    ( formatDay
    , formatWeekday
    , parseMonth
    , monthNumber
    , formatMonth
    )

{- | Utility about date -}

import Date exposing (Date)
import Date
import String
import Time exposing (Weekday(..), Month(..))
import Time

formatDay : Date -> String
formatDay d =
    let result = month ++ "/" ++ day ++ " " ++ weekday
        month = formatMonth <| Date.month d
        day = String.fromInt <| Date.day d
        weekday = "(" ++ (formatWeekday <| Date.weekday d) ++ ")"
    in result

formatWeekday : Weekday -> String
formatWeekday w =
    case w of
        Mon -> "月"
        Tue -> "火"
        Wed -> "水"
        Thu -> "木"
        Fri -> "金"
        Sat -> "土"
        Sun -> "日"

parseMonth : Int -> Result String Month
parseMonth m =
    case m of
        1 ->  Ok Jan
        2 ->  Ok Feb
        3 ->  Ok Mar
        4 ->  Ok Apr
        5 ->  Ok May
        6 ->  Ok Jun
        7 ->  Ok Jul
        8 ->  Ok Aug
        9 ->  Ok Sep
        10 -> Ok Oct
        11 -> Ok Nov
        12 -> Ok Dec
        _ ->  Err ("Invalid month: " ++ String.fromInt m)

monthNumber : Month -> Int
monthNumber m =
    case m of
        Jan -> 1
        Feb -> 2
        Mar -> 3
        Apr -> 4
        May -> 5
        Jun -> 6
        Jul -> 7
        Aug -> 8
        Sep -> 9
        Oct -> 10
        Nov -> 11
        Dec -> 12

formatMonth : Month -> String
formatMonth m = String.fromInt <| monthNumber m
