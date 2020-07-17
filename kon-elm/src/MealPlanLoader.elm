module MealPlanLoader exposing
    ( MealPlanLoader
    , loadInit
    , loadMore
    )

import Date exposing (Date)

import Bridge exposing (BMealPlan)
import Calendar exposing (Calendar)
import Calendar as Cal
import HttpUtil exposing (showHttpError)


{- | Controller for loading MealPlans for Calendar. -}
type MealPlanLoader = MPL

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

{- | Load meal plans for the period specified by the start date and
end date.
-}
loadMore :  Date -- ^ start date (inclusive)
         -> Date -- ^ end date (exclusive)
         -> MealPlanLoader
         -> (MealPlanLoader -> Result String (List BMealPlan) -> msg)
            -- ^ Continuation that receives the loading results.
         -> Cmd msg
            -- ^ Command to load the meal plans.
loadMore start end MPL fmsg =
    let result = loadMealPlans start end handle
        handle mps = fmsg MPL mps
    in result
