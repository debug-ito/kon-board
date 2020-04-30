{-# LANGUAGE DataKinds, TypeOperators #-}
-- |
-- Module: KonBoard.Web.API
-- Description: Web API definitions
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module KonBoard.Web.API
  ( GetMealPlans
  ) where

import Data.Time (Day)
-- import Servant.API
import Servant

import KonBoard.Bridge.MealPlan (BMealPlan)

type GetMealPlans =
  "meal-plans"
  :> QueryParam' [Required, Strict] "start" Day
  :> QueryParam' [Required, Strict] "end" Day
  :> Get '[JSON] [BMealPlan]
