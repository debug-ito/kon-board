module CalEntry exposing
    ( CalEntry
    , fromBMealPlan
    , forDays
    )

import Date exposing (Date)
import Date
import List
import Result
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
