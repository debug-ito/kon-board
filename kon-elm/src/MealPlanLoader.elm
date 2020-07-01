module MealPlanLoader exposing
    ( MealPlanLoader
    , loadInit
    , loadMore
    , isLoading
    )

import Date exposing (Date)

import Bridge exposing (BMealPlan)
import Calendar exposing (Calendar)
import Calendar as Cal
import HttpUtil exposing (showHttpError)


{- | Controller for loading MealPlans -}
type MealPlanLoader = MPL  -- TODO: add internal data structure, if necessary.

{- | Load initial meal plans for the given calendar. This also
constructs a 'MealPlanLoader' object.
-}
loadInit : Calendar -> (Result String (MealPlanLoader, List BMealPlan) -> msg) -> Cmd msg
loadInit cal fmsg =
    let result = loadMealPlans start end handle
        (start, end) = Cal.startAndEnd cal
        handle e_mps = fmsg <| Result.map (\mps -> (MPL, mps)) e_mps
    in result

loadMealPlans : Date -> Date -> (Result String (List BMealPlan) -> msg) -> Cmd msg
loadMealPlans start_day end_day fmsg =
    let handle ret =
            let mkErrorMsg http_err = "Error in loadMealPlans: " ++ showHttpError http_err
            in fmsg <| Result.mapError mkErrorMsg ret
    in Bridge.getApiV1Mealplans (Date.toIsoString start_day) (Date.toIsoString end_day) handle

{- | Return 'True' if it's loading some meal plans now.
-}
isLoading : MealPlanLoader -> Bool
isLoading = Debug.todo "TODO: implement"

{- | Load more meal plans for the calendar. If 'MealPlanLoader' is
currently loading, it returns failure.
-}
loadMore : Calendar
         -> Int
            -- ^ The number of weeks to load. If positive, it loads
            -- meal plans for future of the calendar. If negative, it
            -- loads meal plans for past of the calendar.
         -> MealPlanLoader
         -> ((MealPlanLoader, Result String (List BMealPlan)) -> msg)
            -- ^ Continuation that receives the loading results.
         -> (MealPlanLoader, Cmd msg)
            -- ^ Updated MealPlanLoader and the command to load the meal plans.
loadMore = Debug.todo "TODO: implement"

