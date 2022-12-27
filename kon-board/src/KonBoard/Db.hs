module KonBoard.Db
    (
    ) where

import           Database.Beam (Beamable, C, Database, Identity, PrimaryKey, Table (..),
                                TableEntity)

import           KonBoard.Base (Generic, HasField (..), Text)

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
  data PrimaryKey RecipeT f = RecipeId (C f Integer) deriving (Generic, Beamable)
  primaryKey = RecipeId . getField @"id"

data Db f
  = Db
      { recipes :: f (TableEntity RecipeT)
      }
  deriving (Database be, Generic)

-- TODO: define the database entity.
