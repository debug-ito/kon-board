module Calendar exposing
    ( Calendar
    , CalEntry
    , DayMeal
    , Note
    , mealFor
        
    , MonthAnchor
    , prevMonthAnchor
    , compareMonthAnchors
    , dateToMonthAnchor
        
    , forWeeks
        
    , startAndEnd
    , entries
    , weekEntries
    , oneWeek
    , monthAnchors
    , entryFor
        
    , addMealPlan
    , addMealPlans
    , extend
    , calEntryFromMealPlans
    )

import Date exposing (Date, numberToMonth, monthToNumber)
import Date
import List
import Result
import Result.Extra as ResultE
import Time
import Time exposing (Weekday, Month)

import Bridge exposing (BMealPlan, BRecipeStored)
import DateUtil exposing (parseMonth)
import ListUtil exposing (replaceOrAdd, listToMaybe)
import ListUtil
import MealPhase exposing (MealPhase(..))
import MealPhase

{- | Note on a DayMeal.
-}
type alias Note = String

{- | Meal in a day.
-}
type alias DayMeal =
    { phase : MealPhase
    , recipes : List BRecipeStored
    , notes : List Note
    }
        
{- | Calendar entry
-}
type alias CalEntry =
    { day : Date
    , meals : List DayMeal
    }

emptyCalEntry : Date -> CalEntry
emptyCalEntry d = { day = d, meals = [] }

{- | A beginning of a month.
-}
type alias MonthAnchor =
    { year : Int
    , month : Time.Month
    }

prevMonthAnchor : MonthAnchor -> MonthAnchor
prevMonthAnchor ma =
    let result =
            if mnum == 1
            then { year = ma.year - 1, month = Time.Dec }
            else { year = ma.year, month = numberToMonth (mnum - 1) }
        mnum = monthToNumber ma.month
    in result

compareMonthAnchors : MonthAnchor -> MonthAnchor -> Order
compareMonthAnchors a b =
    let result =
            case order_y of
                EQ -> order_m
                _ -> order_y
        order_y = compare a.year b.year
        order_m = compare (monthToNumber a.month) (monthToNumber b.month)
    in result

{- | If the given 'Date' is the day 1st, return the 'MonthAnchor'.
-}
dateToMonthAnchor : Date -> Maybe MonthAnchor
dateToMonthAnchor d =
    if Date.day d == 1
    then Just { year = Date.year d, month = Date.month d }
    else Nothing

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
    MealPhase.fromString mp.phase |> Result.andThen
    ( \p -> parseMonth mp.month |> Result.andThen
    ( \m ->
          let day = Date.fromCalendarDate mp.year m mp.day
              dm = { phase = p, recipes = mp.recipes, notes = mp.notes }
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


{- | Return the start and end of the calendar. Start date is
inclusive, end date is exclusive.
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

{- | One week of 'Weekday's, starting with the start date of the
calendar.
-}
oneWeek : Calendar -> List Weekday
oneWeek (Calendar c) =
    let result = List.map toWday <| List.range 0 6
        toWday i = Date.numberToWeekday <| (modBy 7 (i + start_wday_idx)) + 1
        start_wday_idx = (Date.weekdayToNumber <| Date.weekday c.start) - 1
    in result

{- | Get list of 'MonthAnchor's in the calendar.
-}
monthAnchors : Calendar -> List MonthAnchor
monthAnchors cl =
    List.filterMap (\ce -> dateToMonthAnchor ce.day) <| entries cl

{- | Get CalEntry for the given date.
-}
entryFor : Date -> Calendar -> Maybe CalEntry
entryFor d cal =
    listToMaybe <| List.filter (\ce -> ce.day == d) <| entries cal

{- | Extend the calendar by the given number of weeks. If `weeks` is
positive, it extends the calendar in the future. If `weeks` is
negative, it extends the calendar in the past. If `weeks` is zero, it
doesn't modify the calendar.

It returns the updated calendar, and start and end dates of the
extended period of days. The start is inclusive, and the end is
exclusive. If `weeks` is zero, both start and end is the start date of
the calendar.
-}
extend : Int -> Calendar -> (Calendar, Date, Date)
extend weeks cal =
    let result =
            if weeks > 0 then
                ret_positive
            else if weeks < 0 then
                ret_negative
            else
                ret_zero
        ret_zero =
            let (orig_start, _) = startAndEnd cal
            in (cal, orig_start, orig_start)
        ret_positive =
            let (Calendar cali) = cal
                new_cal = Calendar { cali | entries = new_entries, end = new_end }
                new_end = Date.add Date.Days (weeks * 7) cali.end
                new_entries = cali.entries ++ added_entries
                added_entries = List.map emptyCalEntry <| Date.range Date.Day 1 cali.end new_end
            in (new_cal, cali.end, new_end)
        ret_negative =
            let (Calendar cali) = cal
                new_cal = Calendar { cali | entries = new_entries, start = new_start }
                new_start = Date.add Date.Days (weeks * 7) cali.start
                new_entries = added_entries ++ cali.entries
                added_entries = List.map emptyCalEntry <| Date.range Date.Day 1 new_start cali.start
            in (new_cal, new_start, cali.start)
    in result

{- | Make a 'CalEntry' from 'BMealPlan's. All meal plans must be for
the given date.
-}
calEntryFromMealPlans : Date -> List BMealPlan -> Result String CalEntry
calEntryFromMealPlans cal_date mps =
    let result =
            Result.map mkCalEntry
            <| Result.andThen (ResultE.combineMap extractDayMeal)
            <| ResultE.combineMap fromBMealPlan mps
        extractDayMeal (mp_date, mp_daymeal) =
            if mp_date == cal_date
            then Ok mp_daymeal
            else Err ( "Expected BMealPlan for " ++ Date.toIsoString cal_date ++ ", but got "
                       ++ Date.toIsoString mp_date
                     )
        mkCalEntry dms = List.foldr setDayMeal (emptyCalEntry cal_date) dms
    in result
