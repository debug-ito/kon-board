module CalEntry exposing
    ( CalEntry
    , fromBMealPlan
    , forDays
    , merge
    , mergeList
    )

import Date exposing (Date)
import Date
import List
import Result
import Result.Extra as ResultE
import Time

import Bridge exposing (BMealPlan, BRecipeSummary)
import MealPhase exposing (MealPhase(..))
import MealPhase
        
{-| Calendar entry
-}
type alias CalEntry =
    { day : Date
    , phase : MealPhase
    , recipeSummary : Maybe BRecipeSummary
    }

fromBMealPlan : BMealPlan -> Result String CalEntry
fromBMealPlan mp =
    MealPhase.parseString mp.phase |> Result.andThen
    ( \p -> parseMonth mp.month |> Result.andThen
    ( \m -> Ok { day = Date.fromCalendarDate mp.year m mp.day
               , phase = p
               , recipeSummary = Just mp.recipe_summary
               }
    ))

parseMonth : Int -> Result String Time.Month
parseMonth m =
    case m of
        1 ->  Ok Time.Jan
        2 ->  Ok Time.Feb
        3 ->  Ok Time.Mar
        4 ->  Ok Time.Apr
        5 ->  Ok Time.May
        6 ->  Ok Time.Jun
        7 ->  Ok Time.Jul
        8 ->  Ok Time.Aug
        9 ->  Ok Time.Sep
        10 -> Ok Time.Oct
        11 -> Ok Time.Nov
        12 -> Ok Time.Dec
        _ ->  Err ("Invalid month: " ++ String.fromInt m)

forDays : Date -> Int -> List CalEntry
forDays start days =
    let makeEnd dif = Date.add Date.Days dif start
        makeCalEntries dif = List.map (\p -> { day = makeEnd dif, phase = p, recipeSummary = Nothing })
                             <| [Lunch, Dinner]
    in List.concatMap makeCalEntries <| List.range 0 days

merge : BMealPlan -> List CalEntry -> Result String (List CalEntry)
merge bm cals  =
    fromBMealPlan bm |> Result.map
    ( \new_cal ->
          let f cal (acc, replaced) = if cal.day == new_cal.day && cal.phase == new_cal.phase
                                      then (new_cal :: acc, True)
                                      else (acc, replaced)
              finalize (ret, replaced) = if replaced
                                         then ret
                                         else new_cal :: ret
          in finalize <| List.foldr f ([], False) cals
    )

mergeList : List BMealPlan -> List CalEntry -> Result String (List CalEntry)
mergeList bps cals =
    let f bp eret =
            case eret of
                Err e -> Err e
                Ok cur_cals -> merge bp cur_cals
    in List.foldr f (Ok cals) bps
