{-# LANGUAGE QuasiQuotes #-}
module KonBoard.Db
    ( Conn
    , openSqlite
    , close
    ) where

import           Data.String.Interpolate                  (i)
import           Database.Beam                            (Beamable, C, Database, DatabaseSettings,
                                                           HasQBuilder, Identity, MonadBeam,
                                                           PrimaryKey, QExpr, Table (..),
                                                           TableEntity, (==.))
import qualified Database.Beam                            as Beam
import           Database.Beam.Backend                    (BeamSqlBackend)
import           Database.Beam.Backend.SQL.BeamExtensions (MonadBeamInsertReturning (..))
import           Database.Beam.Sqlite                     (Sqlite)
import qualified Database.SQLite.Simple                   as SQLite

import           KonBoard.Base                            (Generic, HasField (..), Int32,
                                                           MonadIO (..), MonadThrow, Text,
                                                           throwString)

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
      { id         :: C f Int32
      , name       :: C f Text
      , searchText :: C f Text
      , rawYaml    :: C f Text
      }
  deriving (Generic)

type DbRecipe = DbRecipeT Identity

instance Beamable DbRecipeT

instance Table DbRecipeT where
  data PrimaryKey DbRecipeT f = DbRecipeId (C f Int32) deriving (Generic)
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
  SQLite.execute_ c sqlCreateDbRecipeT

-- TODO: maybe we should enable some pragmas such as auto_vacuum and foreign_keys

insertDbRecipe :: (MonadBeamInsertReturning Sqlite m, MonadThrow m) => DatabaseSettings Sqlite Db -> DbRecipeT (QExpr Sqlite s) -> m Int32
insertDbRecipe db r = fmap (getField @"id") $ takeFirst =<< (runInsertReturningList $ Beam.insertOnly table cols vals)
  where
    table = recipes db
    -- SQLite doesn't support "DEFAULT" for column expression, so we need to specify inserted columns explicitly.
    cols t = (name t, searchText t, rawYaml t)
    vals = Beam.insertData [(name r, searchText r, rawYaml r)]
    takeFirst []    = throwString "get no insert result"
    takeFirst (x:_) = return x

updateDbRecipe :: (MonadBeam Sqlite m) => DatabaseSettings Sqlite Db -> DbRecipe -> m ()
updateDbRecipe db r = Beam.runUpdate $ Beam.save (recipes db) r

getDbRecipeById :: (MonadBeam Sqlite m) => DatabaseSettings Sqlite Db -> Int32 -> m (Maybe DbRecipe)
getDbRecipeById db recipeId = Beam.runSelectReturningOne $ Beam.lookup_ (recipes db) (DbRecipeId recipeId)

getDbRecipeByName :: (MonadBeam Sqlite m) => DatabaseSettings Sqlite Db -> Text -> m (Maybe DbRecipe)
getDbRecipeByName db recipeName = Beam.runSelectReturningOne $ Beam.select query
  where
    query = do
      r <- Beam.all_ $ recipes db
      Beam.guard_ $ getField @"name" r ==. Beam.val_ recipeName
      return r


-- TODO: write Db operation functions.
