module DateUtil exposing
    ( parseMonth
    )

{- | Utility about date -}

import Date exposing (Date)
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
