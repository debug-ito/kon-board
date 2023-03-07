-- |  Web API definitions
module KonBoard.Web.Api
    ( DataApi
    ) where

import           Servant.API

import           Data.Text                (Text)
import           KonBoard.Bridge.MealPlan (BMealPlan)
import           KonBoard.Bridge.Recipe   (BRecipeId, BRecipeStored)
import           KonBoard.Bridge.Time     (BDay)

type DataApi = "api" :> "v1" :>
               ( GetMealPlans
                 :<|> GetRecipe
               )

type GetMealPlans =
  "meal-plans"
  :> QueryParam' [Required, Strict] "start" BDay
  :> QueryParam' [Required, Strict] "end" BDay
  :> Get '[JSON] [BMealPlan]

type GetRecipe =
  "recipes"
  :> Capture "recipe-id" BRecipeId
  :> Get '[JSON] BRecipeStored
