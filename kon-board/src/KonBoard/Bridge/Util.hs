-- |
-- Module: KonBoard.Bridge.Util
-- Description: Common utility for bridges
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module KonBoard.Bridge.Util
  ( dropLabelOptions
  ) where

import Data.Aeson
  ( Options(fieldLabelModifier, sumEncoding, omitNothingFields), defaultOptions,
    SumEncoding(..)
  )

dropLabelOptions :: Int -> Options
dropLabelOptions drop_num =
  defaultOptions
  { fieldLabelModifier = drop drop_num,
    sumEncoding = ObjectWithSingleField,
    omitNothingFields = True
  }
