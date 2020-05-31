module DateUtil exposing
    ( parseMonth
    , nearbyWeekday
    )

{- | Utility about date -}

import Date exposing (Date, Unit(..))
import Date
import String
import Time exposing (Weekday(..), Month(..))
import Time

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

{- | Return the date of the given 'Weekday' near the given starting
'Date'. It returns the date of the n-th week from the starting
date. If n > 0, it returns the date in future. If n < 0, it returns
the date in past. n == 0 is unexpected, and it returns the starting
date.
-}
nearbyWeekday : Date -> Weekday -> Int -> Date
nearbyWeekday start target_wday nth =
    let result =
            if nth == 0
            then start
            else if nth > 0
                 then nearbyWeekday (Date.add Days diff_plus start)  target_wday (nth - 1)
                 else nearbyWeekday (Date.add Days diff_minus start) target_wday (nth + 1)
        diff_plus =
            let orig = diffWeekday target_wday (Date.weekday start)
            in if orig == 0 then 7 else orig
        diff_minus = (diffWeekday target_wday (Date.weekday start)) - 7
    in result

{- | Return diff of "a - b" in number of days. The return value is in [0,6].
-}
diffWeekday : Weekday -> Weekday -> Int
diffWeekday a b =
    let result = wrapNegative (na - nb)
        na = Date.weekdayToNumber a
        nb = Date.weekdayToNumber b
        wrapNegative d = if d < 0
                         then wrapNegative (d + 7)
                         else d
    in result
