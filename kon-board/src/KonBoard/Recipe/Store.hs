-- |
-- Module: KonBoard.Recipe.Store
-- Description: Storage service for Recipes
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module KonBoard.Recipe.Store
  ( -- * RecipeSummary
    RecipeSummary(..),
    ID,
    toRecipeSummary,
    -- * RecipeStore
    RecipeStore,
    openYAMLs,
    loadRecipeByName,
    loadRecipe,
    loadRecipeSummary
  ) where

import Data.Text (Text)

import KonBoard.Recipe (Recipe(recipeName), Name)

-- | Handle for the recipe storage.
data RecipeStore = RecipeStore -- TODO

-- | URL-fiendly ID for a recipe
type ID = Text

-- | Consise summary of a 'Recipe'.
data RecipeSummary =
  RecipeSummary
  { rsID :: ID,
    rsName :: Name
  }
  deriving (Show,Eq,Ord)

toRecipeSummary :: ID -> Recipe -> RecipeSummary
toRecipeSummary rid r = RecipeSummary rid $ recipeName r

loadRecipeByName :: RecipeStore -> Name -> IO (Maybe RecipeSummary)
loadRecipeByName = undefined -- TODO

loadRecipe :: RecipeStore -> ID -> IO Recipe
loadRecipe = undefined -- TODO

loadRecipeSummary :: RecipeStore -> ID -> IO RecipeSummary
loadRecipeSummary store rid = fmap (toRecipeSummary rid) $ loadRecipe store rid

-- | Open YAML files and make a 'RecipeStore'.
openYAMLs :: [FilePath] -> IO RecipeStore
openYAMLs = undefined
