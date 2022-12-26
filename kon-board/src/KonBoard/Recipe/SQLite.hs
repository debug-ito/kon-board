module KonBoard.Recipe.SQLite
    (
    ) where

import           Database.Beam (Beamable, C, Identity, PrimaryKey, Table)

import           KonBoard.Base (Generic, HasField (..))

data RecipeT f
  = Recipe
      { id         :: C f Integer
      , name       :: C f Text
      , searchText :: C f Text
      , rawYaml    :: C f Text
      }
  deriving (Generic)

type Recipe = RecipeT Identity

instance Beamable RecipeT

instance Table RecipeT where
  data PrimaryKey RecipeT f = RecipeId (C f Integer)
  primaryKey = RecipeId . getField @"id"

-- TODO: define the database entity.
