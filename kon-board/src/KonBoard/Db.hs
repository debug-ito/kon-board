module KonBoard.Db
    ( Conn
    , openSqlite
    , close
    ) where

import           Database.Beam          (Beamable, C, Database, Identity, PrimaryKey, Table (..),
                                         TableEntity)
import qualified Database.SQLite.Simple as SQLite

import           KonBoard.Base          (Generic, HasField (..), MonadIO (..), Text)

sqlCreateDbRecipeT :: SQLite.Query
sqlCreateDbRecipeT = [i|
CREATE TABLE IF NOT EXISTS recipes (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  name TEXT NOT NULL UNIQUE,
  search_text TEXT NOT NULL,
  rawYaml TEXT NOT NULL
)|]

data DbRecipeT f
  = DbRecipe
      { id         :: C f Integer
      , name       :: C f Text
      , searchText :: C f Text
      , rawYaml    :: C f Text
      }
  deriving (Generic)

type DbRecipe = DbRecipeT Identity

instance Beamable DbRecipeT

instance Table DbRecipeT where
  data PrimaryKey DbRecipeT f = DbRecipeId (C f Integer) deriving (Generic)
  primaryKey = DbRecipeId . getField @"id"

instance Beamable (PrimaryKey DbRecipeT)

data Db f
  = Db
      { recipes :: f (TableEntity DbRecipeT)
      }
  deriving (Generic)

instance Database be Db

data Conn
  = Conn SQLite.Connection

openSqlite :: MonadIO m => FilePath -> m Conn
openSqlite f = liftIO $ do
  conn <- fmap Conn $ SQLite.open f
  initDb conn
  return conn

close :: MonadIO m => Conn -> m ()
close (Conn c) = liftIO $ SQLite.close c

initDb :: Conn -> IO ()
initDb (Conn c) = do
  SQLite.execute_ sqlCreateDbRecipeT

-- TODO: maybe we should enable some pragmas such as auto_vacuum and foreign_keys

insertDbRecipe :: Conn -> DbRecipe f -> IO Integer
insertDbRecipe = undefined -- TODO: what type should we use for @f@ ?? The id is autoincremented, and we must retrieve that value.


-- TODO: write Db operation functions.
