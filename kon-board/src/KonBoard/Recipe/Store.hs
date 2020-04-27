-- |
-- Module: KonBoard.Recipe.Store
-- Description: Storage service for Recipes
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module KonBoard.Recipe.Store
  ( RecipeStore,
    ID,
    loadRecipe
  ) where

import Data.Text (Text)

import KonBoard.Recipe (Recipe)

-- | Handle for the recipe storage.
data RecipeStore = RecipeStore

-- | URL-fiendly ID for a recipe
type ID = Text

loadRecipe :: RecipeStore -> ID -> IO Recipe
loadRecipe = undefined -- TODO
