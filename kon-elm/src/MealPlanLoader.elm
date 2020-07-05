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
type MealPlanLoader = MPL MPLImpl

type alias MPLImpl =
    { isLoading : Bool
    }

{- | Load initial meal plans for the given calendar. This also
constructs a 'MealPlanLoader' object.
-}
loadInit : Calendar -> (Result String (MealPlanLoader, List BMealPlan) -> msg) -> Cmd msg
loadInit cal fmsg =
    let result = loadMealPlans start end handle
        (start, end) = Cal.startAndEnd cal
        handle e_mps = fmsg <| Result.map (\mps -> (MPL { isLoading = False }, mps)) e_mps
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
isLoading (MPL m) = m.isLoading

{- | Load meal plans for the period specified by the start date and
end date. If 'MealPlanLoader' is currently loading, it returns
failure.
-}
loadMore :  Date -- ^ start date (inclusive)
         -> Date -- ^ end date (exclusive)
         -> MealPlanLoader
         -> (MealPlanLoader -> Result String (List BMealPlan) -> msg)
            -- ^ Continuation that receives the loading results.
         -> Result String (MealPlanLoader, Cmd msg)
            -- ^ Updated MealPlanLoader and the command to load the meal plans.
loadMore start end mpl fmsg =
    let result =
            if isLoading mpl then
                Err ("MealPlanLoader is currently loading something. Abort.")
            else
                Ok (next_mpl, load_cmd)
        next_mpl = MPL next_mpli
        next_mpli =
            case mpl of
                MPL m -> { m | isLoading = True }
        load_cmd = loadMealPlans start end handle
        handle ret = fmsg (MPL { next_mpli | isLoading = False }) ret
    in result

