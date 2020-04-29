{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module: KonBoard.Bridge.Recipe
-- Description: Easy-to-encode Recipe data types
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module KonBoard.Bridge.Recipe
  ( BRecipeSummary(..),
    toBRecipeSummary,
    fromBRecipeSummary
  ) where

import Data.Text (Text)
import qualified Elm.Derive as Elm

import KonBoard.Bridge.Util (dropLabelOptions)
import KonBoard.Recipe.Store
  ( RecipeSummary(..)
  )

-- | Easy-to-encode version of 'RecipeSummary'.
data BRecipeSummary =
  BRecipeSummary
  { br_id :: Text,
    br_name :: Text
  }
  deriving (Show,Eq,Ord)

$(Elm.deriveBoth (dropLabelOptions 3) ''BRecipeSummary)

toBRecipeSummary :: RecipeSummary -> BRecipeSummary
toBRecipeSummary (RecipeSummary i n) = BRecipeSummary i n

fromBRecipeSummary :: BRecipeSummary -> RecipeSummary
fromBRecipeSummary (BRecipeSummary i n) = RecipeSummary i n
