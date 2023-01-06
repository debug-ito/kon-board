{-# LANGUAGE QuasiQuotes #-}
module KonBoard.Db
    ( Conn
    , openSqlite
    , close
    , recipeStoreDb
    ) where

import           Data.String.Interpolate                  (i)
import qualified Data.Text                                as T
import qualified Data.Text.Read                           as TRead
import           Database.Beam                            (Beamable, C, Database, DatabaseSettings,
                                                           HasQBuilder, Identity, MonadBeam,
                                                           PrimaryKey, QExpr, Table (..),
                                                           TableEntity, (==.))
import qualified Database.Beam                            as Beam
import           Database.Beam.Backend                    (BeamSqlBackend)
import           Database.Beam.Backend.SQL.BeamExtensions (MonadBeamInsertReturning (..))
import           Database.Beam.Sqlite                     (Sqlite, SqliteM, runBeamSqlite)
import qualified Database.SQLite.Simple                   as SQLite

import           KonBoard.Base                            (ByteString, Generic, HasField (..),
                                                           Int32, MonadIO (..), MonadThrow, Text,
                                                           throwString)
import           KonBoard.Db.Orphans                      ()
import           KonBoard.Recipe                          (Id, IngDesc (..), Ingredient (..),
                                                           Recipe, RecipeStore (..),
                                                           RecipeStored (..), Ref (..), parseRecipe)


sqlCreateDbRecipeT :: SQLite.Query
sqlCreateDbRecipeT = [i|
CREATE TABLE IF NOT EXISTS recipes (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  name TEXT NOT NULL UNIQUE,
  search_text TEXT NOT NULL,
  rawYaml BLOB NOT NULL
)|]

data DbRecipeT f
  = DbRecipe
      { id         :: C f Int32
      , name       :: C f Text
      , searchText :: C f Text
      , rawYaml    :: C f ByteString
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

recipeStoreDb :: (MonadIO m, MonadThrow m) => Conn -> RecipeStore m
recipeStoreDb (Conn c) =
  RecipeStore
  { insertRecipe = \r -> liftIO $ runBeamSqlite c $ fmap toRecipeId $ insertDbRecipe $ toDbRecipe r
  , updateRecipe = \r -> do
      dbR <- liftIO $ toDbRecipeStored r
      liftIO $ runBeamSqlite c $ updateDbRecipe dbR
  , getRecipeById = \rId -> do
      dbrId <- fromRecipeId rId
      traverse fromDbRecipe =<< (liftIO $ runBeamSqlite c $ getDbRecipeById dbrId)
  , getRecipeByName = \n -> traverse fromDbRecipe =<< (liftIO $ runBeamSqlite c $ getDbRecipeByName n)
  }

initDb :: Conn -> IO ()
initDb (Conn c) = do
  SQLite.execute_ c sqlCreateDbRecipeT

-- TODO: maybe we should enable some pragmas such as auto_vacuum and foreign_keys

dbSettings :: DatabaseSettings be Db
dbSettings = Beam.defaultDbSettings

makeSearchText :: Recipe -> Text
makeSearchText r = T.intercalate "\n" elements
  where
    elements = [getField @"name" r, getField @"description" r] ++ foodItems ++ refs
    foodItems = selectFoodItem =<< getField @"ingredients" r
    selectFoodItem (IngSingle (Ingredient f _)) = [f]
    selectFoodItem (IngGroup _ ings)            = map (\(Ingredient f _) -> f) ings
    refs = selectRefSource =<< getField @"references" r
    selectRefSource (RefSource t) = [t]
    selectRefSource _             = []


toDbRecipe :: Recipe -> DbRecipeT (QExpr Sqlite s)
toDbRecipe r =
  DbRecipe { id = Beam.default_
           , name = Beam.val_ $ getField @"name" r
           , searchText = Beam.val_ $ makeSearchText r
           , rawYaml = Beam.val_ $ getField @"rawYaml" r
           }

toDbRecipeStored :: (MonadThrow m) => RecipeStored -> m DbRecipe
toDbRecipeStored rs = do
  rId <- fromRecipeId $ getField @"id" rs
  return $ DbRecipe { id = rId
                    , name = getField @"name" r
                    , searchText = makeSearchText r
                    , rawYaml = getField @"rawYaml" r
                    }
  where
    r = getField @"recipe" rs

fromDbRecipe :: (MonadThrow m) => DbRecipe -> m RecipeStored
fromDbRecipe dbR = do
  r <- either throwString return $ parseRecipe $ getField @"rawYaml" dbR
  return $ RecipeStored { id = toRecipeId $ getField @"id" dbR, recipe = r }

toRecipeId :: Int32 -> Id
toRecipeId = T.pack . show

fromRecipeId :: (MonadThrow m) => Id -> m Int32
fromRecipeId rId = either throwString return $ fmap fst $ TRead.decimal rId

insertDbRecipe :: (MonadBeamInsertReturning Sqlite m, MonadThrow m) => DbRecipeT (QExpr Sqlite s) -> m Int32
insertDbRecipe r = fmap (getField @"id") $ takeFirst =<< (runInsertReturningList $ Beam.insertOnly table cols vals)
  where
    table = recipes dbSettings
    -- SQLite doesn't support "DEFAULT" for column expression, so we need to specify inserted columns explicitly.
    cols t = (name t, searchText t, rawYaml t)
    vals = Beam.insertData [(name r, searchText r, rawYaml r)]
    takeFirst []    = throwString "get no insert result"
    takeFirst (x:_) = return x

updateDbRecipe :: (MonadBeam Sqlite m) => DbRecipe -> m ()
updateDbRecipe r = Beam.runUpdate $ Beam.save (recipes dbSettings) r

getDbRecipeById :: (MonadBeam Sqlite m) => Int32 -> m (Maybe DbRecipe)
getDbRecipeById recipeId = Beam.runSelectReturningOne $ Beam.lookup_ (recipes dbSettings) (DbRecipeId recipeId)

getDbRecipeByName :: (MonadBeam Sqlite m) => Text -> m (Maybe DbRecipe)
getDbRecipeByName recipeName = Beam.runSelectReturningOne $ Beam.select query
  where
    query = do
      r <- Beam.all_ $ recipes dbSettings
      Beam.guard_ $ getField @"name" r ==. Beam.val_ recipeName
      return r
