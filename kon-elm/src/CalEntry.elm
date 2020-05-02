module CalEntry exposing
    ( CalEntry
    , DayMeal
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
import ListUtil exposing (replaceOrAdd)
import MealPhase exposing (MealPhase(..))
import MealPhase

{-| Meal in a day.
-}
type alias DayMeal =
    { phase : MealPhase
    , recipes : List BRecipeSummary
    }
        
{-| Calendar entry
-}
type alias CalEntry =
    { day : Date
    , meals : List DayMeal
    }

fromBMealPlan : BMealPlan -> Result String (Date, DayMeal)
fromBMealPlan mp =
    MealPhase.parseString mp.phase |> Result.andThen
    ( \p -> parseMonth mp.month |> Result.andThen
    ( \m ->
          let day = Date.fromCalendarDate mp.year m mp.day
              dm = { phase = p, recipes = mp.recipes }
          in Ok (day, dm)

mealFor : Date -> MealPhase -> CalEntry -> Maybe DayMeal
mealFor d mp cal =
    if cal.day != d
    then Nothing
    else List.head <| List.filter (\dm -> dm.phase == mp) cal.meals

setDayMeal : DayMeal -> CalEntry -> CalEntry
setDayMeal new_dm cal =
    let new_cal = { cal | meals = new_meals }
        new_meals = replaceOrAdd p new_dm cal.meals
        p cur_dm = cur_dm.phase == new_dm.phase

---         finalize <| foldr f ([], False) cal.meals
---         f dm (acc, replaced) =
---             if dm.phase == new_dm.phase
---             then (new_dm :: acc, True)
---             else (dm     :: acc, replaced)
---         finalize (ret, replaced) =
---             if replaced
---             then ret
---             else (new_dm :: ret)
---     in new_cal

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
        makeCalEntries dif = [{ day = makeEnd dif, meals = [] }]
        -- makeCalEntries dif = List.map (\p -> { day = makeEnd dif, phase = p, recipeSummaries = [] })
        --                      <| [Lunch, Dinner]
    in List.concatMap makeCalEntries <| List.range 0 (days - 1)

-- TODO: fix this! use mealFor and setDayMeal.

-- maybe we should make: fromBMealPlan : BMealPlan -> Result String (Date, DayMeal) ??

merge : BMealPlan -> List CalEntry -> Result String (List CalEntry)
merge bm cals  =
    fromBMealPlan bm |> Result.map
    ( \new_cal ->

---           let f cal (acc, replaced) = if cal.day == new_cal.day && cal.phase == new_cal.phase
---                                       then (new_cal :: acc, True)
---                                       else (cal :: acc, replaced)
---               finalize (ret, replaced) = if replaced
---                                          then ret
---                                          else new_cal :: ret
---           in finalize <| List.foldr f ([], False) cals
    )

mergeList : List BMealPlan -> List CalEntry -> Result String (List CalEntry)
mergeList bps cals =
    let f bp eret =
            case eret of
                Err e -> Err e
                Ok cur_cals -> merge bp cur_cals
    in List.foldr f (Ok cals) bps
