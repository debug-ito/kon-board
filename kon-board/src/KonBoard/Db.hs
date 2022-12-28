module KonBoard.Db
    ( Conn
    , openSqlite
    , close
    ) where

import           Database.Beam          (Beamable, C, Database, Identity, PrimaryKey, Table (..),
                                         TableEntity)
import qualified Database.SQLite.Simple as SQLite

import           KonBoard.Base          (Generic, HasField (..), MonadIO (..), Text)

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
  data PrimaryKey RecipeT f = RecipeId (C f Integer) deriving (Generic)
  primaryKey = RecipeId . getField @"id"

instance Beamable (PrimaryKey RecipeT)

data Db f
  = Db
      { recipes :: f (TableEntity RecipeT)
      }
  deriving (Generic)

instance Database be Db

data Conn
  = Conn SQLite.Connection

openSqlite :: MonadIO m => FilePath -> m Conn
openSqlite f = liftIO $ fmap Conn $ SQLite.open f

close :: MonadIO m => Conn -> m ()
close (Conn c) = liftIO $ SQLite.close c

-- TODO: write Db operation functions.
