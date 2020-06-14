module Calendar exposing
    ( Calendar
    , CalEntry
    , DayMeal
    , forWeeks
    , startAndEnd
    , entries
    , weekEntries
    , addMealPlan
    , addMealPlans
    , mealFor
    )

import Date exposing (Date)
import Date
import List
import Result
import Result.Extra as ResultE
import Time
import Time exposing (Weekday)

import Bridge exposing (BMealPlan, BRecipeSummary)
import DateUtil exposing (parseMonth)
import ListUtil exposing (replaceOrAdd)
import ListUtil
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

{- | Opaque Calendar
-}
type Calendar = Calendar CalImpl

{- | Internal implementation of Calendar
-}
type alias CalImpl =
    { entries : List CalEntry
    , start : Date
    , end : Date
    }

fromBMealPlan : BMealPlan -> Result String (Date, DayMeal)
fromBMealPlan mp =
    MealPhase.parseString mp.phase |> Result.andThen
    ( \p -> parseMonth mp.month |> Result.andThen
    ( \m ->
          let day = Date.fromCalendarDate mp.year m mp.day
              dm = { phase = p, recipes = mp.recipes }
          in Ok (day, dm)
    ) )

mealFor : MealPhase -> CalEntry -> Maybe DayMeal
mealFor mp cal = List.head <| List.filter (\dm -> dm.phase == mp) cal.meals

setDayMeal : DayMeal -> CalEntry -> CalEntry
setDayMeal new_dm cal =
    let new_cal = { cal | meals = new_meals }
        new_meals = replaceOrAdd p new_dm cal.meals
        p cur_dm = cur_dm.phase == new_dm.phase
    in new_cal

{- | Make a 'Calendar' that centers at the given date and spans for
given number of weeks in future and past. The start_wday is the
starting 'Weekday' in the calendar.
-}
forWeeks : Date -> Weekday -> Int -> Int -> Calendar
forWeeks center_date start_wday weeks_future weeks_past = 
    let result = forDays start_date end_date
        start_date = DateUtil.nearbyWeekday center_date start_wday (negate weeks_past)
        end_date   = DateUtil.nearbyWeekday center_date start_wday weeks_future
    in result

forDays : Date -> Date -> Calendar
forDays start_date end_date =
    let result = Calendar
                 { entries = cal_entries
                 , start = start_date
                 , end = end_date
                 }
        makeEnd dif = Date.add Date.Days dif start_date
        makeCalEntries dif = [{ day = makeEnd dif, meals = [] }]
        duration = Date.diff Date.Days start_date end_date
        cal_entries = List.concatMap makeCalEntries <| List.range 0 (duration - 1)
    in result

addMealPlan : BMealPlan -> Calendar -> Result String Calendar
addMealPlan bm (Calendar cal)  =
    fromBMealPlan bm |> Result.andThen
    ( \(new_date, new_dm) ->
          let result = finalize <| List.foldr f ([], False) cal.entries
              f cur_entry (acc, is_complete) =
                  if is_complete
                  then (cur_entry :: acc, is_complete)
                  else if cur_entry.day == new_date 
                       then (setDayMeal new_dm cur_entry :: acc, True)
                       else (cur_entry :: acc, is_complete)
              finalize (ret_entries, is_complete) =
                  if is_complete
                  then Ok <| Calendar { cal | entries = ret_entries }
                  else
                      let err_msg = "Cannot find CalEntry for " ++ Date.toIsoString new_date
                      in Err err_msg
          in result
    )

addMealPlans : List BMealPlan -> Calendar -> Result String Calendar
addMealPlans bps cal =
    let f bp eret =
            case eret of
                Err e -> Err e
                Ok cur_cal -> addMealPlan bp cur_cal
    in List.foldr f (Ok cal) bps

{- | Return the start and end of the calendar.
-}
startAndEnd : Calendar -> (Date, Date)
startAndEnd (Calendar c) = (c.start, c.end)

{- | Get all 'CalEntry's in 'Calendar', with increasing date and meal
phase.
-}
entries : Calendar -> List CalEntry
entries (Calendar c) = c.entries

{- | Get all 'CalEntry's in 'Calendar', grouped by week.
-}
weekEntries : Calendar -> List (List CalEntry)
weekEntries (Calendar c) = ListUtil.blocks 7 c.entries