module MealPlanLoader exposing
    ( MealPlanLoader
    , loadInit
    , loadMore
    , isLoading
    )

import Bridge exposing (BMealPlan)
import Calendar exposing (Calendar)


{- | Controller for loading MealPlans -}
type MealPlanLoader = MPL MPLImpl

{- | Load initial meal plans for the given calendar. This also
constructs a 'MealPlanLoader' object.
-}
loadInit : Calendar -> (Result String (MealPlanLoader, List BMealPlan) -> msg) -> Cmd msg
loadInit = Debug.todo "implement"

{- | Return 'True' if it's loading some meal plans now.
-}
isLoading : MealPlanLoader -> Bool
isLoading = Debug.todo "implement"

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
loadMore = Debug.todo "implement"

