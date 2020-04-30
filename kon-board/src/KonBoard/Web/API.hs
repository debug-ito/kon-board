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

-- import Servant.API
import Servant

import KonBoard.Bridge.MealPlan (BMealPlan)
import KonBoard.Bridge.Time (BDay)

type GetMealPlans =
  "meal-plans"
  :> QueryParam' [Required, Strict] "start" BDay
  :> QueryParam' [Required, Strict] "end" BDay
  :> Get '[JSON] [BMealPlan]
