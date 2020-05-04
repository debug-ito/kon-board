{-# LANGUAGE DataKinds, TypeOperators #-}
-- |
-- Module: KonBoard.Web.API
-- Description: Web API definitions
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module KonBoard.Web.API
  ( DataAPI
  ) where

-- import Servant.API
import Servant

import Data.Text (Text)
import KonBoard.Bridge.Recipe (BRecipe)
import KonBoard.Bridge.MealPlan (BMealPlan)
import KonBoard.Bridge.Time (BDay)

type DataAPI = "api" :> "v1" :>
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
  :> Capture "recipe-id" Text
  :> Get '[JSON] BRecipe
