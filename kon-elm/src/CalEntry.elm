module CalEntry exposing
    ( Calendar
    , CalEntry
    , DayMeal
    , forDays
    , addMealPlan
    , addMealPlans
    )

import Date exposing (Date)
import Date
import List
import Result
import Result.Extra as ResultE
import Time

import Bridge exposing (BMealPlan, BRecipeSummary)
import DateUtil exposing (parseMonth)
import ListUtil exposing (replaceOrAdd)
import MealPhase exposing (MealPhase(..))
import MealPhase

{- | Meal in a day.
-}
type alias DayMeal =
    { phase : MealPhase
    , recipes : List BRecipeSummary
    }
        
{- | Calendar entry
-}
type alias CalEntry =
    { day : Date
    , meals : List DayMeal
    }

{- | Calendar
-}
type alias Calendar = List CalEntry

fromBMealPlan : BMealPlan -> Result String (Date, DayMeal)
fromBMealPlan mp =
    MealPhase.parseString mp.phase |> Result.andThen
    ( \p -> parseMonth mp.month |> Result.andThen
    ( \m ->
          let day = Date.fromCalendarDate mp.year m mp.day
              dm = { phase = p, recipes = mp.recipes }
          in Ok (day, dm)
    ) )

mealFor : Date -> MealPhase -> CalEntry -> Maybe DayMeal
mealFor d mp cal =
    if cal.day /= d
    then Nothing
    else List.head <| List.filter (\dm -> dm.phase == mp) cal.meals

setDayMeal : DayMeal -> CalEntry -> CalEntry
setDayMeal new_dm cal =
    let new_cal = { cal | meals = new_meals }
        new_meals = replaceOrAdd p new_dm cal.meals
        p cur_dm = cur_dm.phase == new_dm.phase
    in new_cal

forDays : Date -> Int -> Calendar
forDays start days =
    let makeEnd dif = Date.add Date.Days dif start
        makeCalEntries dif = [{ day = makeEnd dif, meals = [] }]
    in List.concatMap makeCalEntries <| List.range 0 (days - 1)

addMealPlan : BMealPlan -> Calendar -> Result String Calendar
addMealPlan bm cals  =
    fromBMealPlan bm |> Result.andThen
    ( \(new_date, new_dm) ->
          let result = finalize <| List.foldr f ([], False) cals
              f cur_cal (acc, is_complete) =
                  if is_complete
                  then (cur_cal :: acc, is_complete)
                  else if cur_cal.day == new_date 
                       then (setDayMeal new_dm cur_cal :: acc, True)
                       else (cur_cal :: acc, is_complete)
              finalize (ret, is_complete) =
                  if is_complete
                  then Ok ret
                  else
                      let err_msg = "Cannot find CalEntry for " ++ Date.toIsoString new_date
                      in Err err_msg
          in result
    )

addMealPlans : List BMealPlan -> Calendar -> Result String Calendar
addMealPlans bps cals =
    let f bp eret =
            case eret of
                Err e -> Err e
                Ok cur_cals -> addMealPlan bp cur_cals
    in List.foldr f (Ok cals) bps
