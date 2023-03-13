{-# LANGUAGE QuasiQuotes #-}
module KonBoard.Db
    ( Conn
    , newSqliteConn
    , close
    , recipeStoreDb
    ) where

import qualified Data.String.Interpolate                  as I
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
sqlCreateDbRecipeT = [I.i|
CREATE TABLE IF NOT EXISTS recipes (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  name TEXT NOT NULL UNIQUE,
  search_text TEXT NOT NULL,
  raw_yaml BLOB NOT NULL
)|]

-- TODO: should we add created_at and updated_at columns??

data DbRecipeT f
  = DbRecipe
      { rId         :: C f Int32
      , rName       :: C f Text
      , rSearchText :: C f Text
      , rRawYaml    :: C f ByteString
      }
  deriving (Generic)

type DbRecipe = DbRecipeT Identity

instance Beamable DbRecipeT

instance Table DbRecipeT where
  data PrimaryKey DbRecipeT f = DbRecipeId (C f Int32) deriving (Generic)
  primaryKey = DbRecipeId . getField @"rId"

instance Beamable (PrimaryKey DbRecipeT)

data Db f
  = Db
      { recipes :: f (TableEntity DbRecipeT)
      }
  deriving (Generic)

instance Database be Db

data Conn
  = Conn SQLite.Connection

newSqliteConn :: MonadIO m => FilePath -> m Conn
newSqliteConn f = liftIO $ do
  conn <- fmap Conn $ SQLite.open f
  initDb conn
  return conn

close :: MonadIO m => Conn -> m ()
close (Conn c) = liftIO $ SQLite.close c

recipeStoreDb :: (MonadIO m, MonadThrow m) => Conn -> RecipeStore m
recipeStoreDb (Conn c) =
  RecipeStore
  { addRecipe = \r -> liftIO $ runBeamSqlite c $ fmap toRecipeId $ addDbRecipe $ toDbRecipe r
  , updateRecipe = \r -> do
      dbR <- liftIO $ toDbRecipeStored r
      liftIO $ runBeamSqlite c $ updateDbRecipe dbR
  , getRecipeById = \i -> do
      dbrId <- fromRecipeId i
      traverse fromDbRecipe =<< (liftIO $ runBeamSqlite c $ getDbRecipeById dbrId)
  , getRecipeByName = \n -> traverse fromDbRecipe =<< (liftIO $ runBeamSqlite c $ getDbRecipeByName n)
  }

initDb :: Conn -> IO ()
initDb (Conn c) =
  mapM_ (SQLite.execute_ c)
  [ sqlCreateDbRecipeT
  , sqlCreateDbMealPlanHeaderT
  , sqlCreateDbMealPlanRecipeT
  , sqlCreateDbMealPlanNoteT
  ]


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
  DbRecipe { rId = Beam.default_
           , rName = Beam.val_ $ getField @"name" r
           , rSearchText = Beam.val_ $ makeSearchText r
           , rRawYaml = Beam.val_ $ getField @"rawYaml" r
           }

toDbRecipeStored :: (MonadThrow m) => RecipeStored -> m DbRecipe
toDbRecipeStored rs = do
  ri <- fromRecipeId $ getField @"id" rs
  return $ DbRecipe { rId = ri
                    , rName = getField @"name" r
                    , rSearchText = makeSearchText r
                    , rRawYaml = getField @"rawYaml" r
                    }
  where
    r = getField @"recipe" rs

fromDbRecipe :: (MonadThrow m) => DbRecipe -> m RecipeStored
fromDbRecipe dbR = do
  r <- either throwString return $ parseRecipe $ getField @"rRawYaml" dbR
  return $ RecipeStored { id = toRecipeId $ getField @"rId" dbR, recipe = r }

toRecipeId :: Int32 -> Id
toRecipeId = T.pack . show

fromRecipeId :: (MonadThrow m) => Id -> m Int32
fromRecipeId ri = either throwString return $ fmap fst $ TRead.decimal ri

addDbRecipe :: (MonadBeamInsertReturning Sqlite m, MonadThrow m) => DbRecipeT (QExpr Sqlite s) -> m Int32
addDbRecipe r = fmap (getField @"rId") $ takeFirst =<< (runInsertReturningList $ Beam.insertOnly table cols vals)
  where
    table = recipes dbSettings
    -- SQLite doesn't support "DEFAULT" for column expression, so we need to specify inserted columns explicitly.
    cols t = (rName t, rSearchText t, rRawYaml t)
    vals = Beam.insertData [(rName r, rSearchText r, rRawYaml r)]
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
      Beam.guard_ $ getField @"rName" r ==. Beam.val_ recipeName
      return r

----------------------------------------------------------------

sqlCreateDbMealPlanHeaderT :: SQLite.Query
sqlCreateDbMealPlanHeaderT = [I.i|
CREATE TABLE IF NOT EXISTS meal_plan_headers (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  year INTEGER NOT NULL,
  month INTEGER NOT NULL,
  day INTEGER NOT NULL,
  phase TEXT NOT NULL,
  UNIQUE (year, month, day, phase)
)
|]

data DbMealPlanHeaderT f
  = DbMealPlanHeader
      { mId    :: C f Int32
      , mYear  :: C f Int32
      , mMonth :: C f Int32
      , mDay   :: C f Int32
      , mPhase :: C f Text
      }
  deriving (Generic)

instance Beamable DbMealPlanHeaderT

instance Table DbMealPlanHeaderT where
  data PrimaryKey DbMealPlanHeaderT f = DbMealPlanHeaderId (C f Int32) deriving (Generic)
  primaryKey = DbMealPlanHeaderId . getField @"mId"

instance Beamable (PrimaryKey DbMealPlanHeaderT)


sqlCreateDbMealPlanRecipeT :: SQLite.Query
sqlCreateDbMealPlanRecipeT = [I.i|
CREATE TABLE IF NOT EXISTS meal_plan_recipes (
  meal_plan_id INTEGER NOT NULL REFERENCES meal_plan_headers (id) ON DELETE CASCADE ON UPDATE CASCADE,
  list_index INTEGER NOT NULL,
  recipe_id INTEGER NOT NULL REFERENCES recipes (id) ON DELETE CASCADE ON UPDATE CASCADE,
  PRIMARY KEY (meal_plan_id, list_index)
)
|]

data DbMealPlanRecipeT f
  = DbMealPlanRecipe
      { mMealPlanId :: C f Int32
      , mListIndex  :: C f Int32
      , mRecipeId   :: C f Int32
      }
  deriving (Generic)

instance Beamable DbMealPlanRecipeT

instance Table DbMealPlanRecipeT where
  data PrimaryKey DbMealPlanRecipeT f = DbMealPlanRecipeId (C f Int32) (C f Int32) deriving (Generic)
  primaryKey = DbMealPlanRecipeId <$> getField @"mMealPlanId" <*> getField @"mListIndex"

instance Beamable (PrimaryKey DbMealPlanRecipeT) where

sqlCreateDbMealPlanNoteT :: SQLite.Query
sqlCreateDbMealPlanNoteT = [I.i|
CREATE TABLE IF NOT EXISTS meal_plan_notes (
  meal_plan_id INTEGER NOT NULL REFERENCES meal_plan_headers (id) ON DELETE CASCADE ON UPDATE CASCADE,
  list_index INTEGER NOT NULL,
  note TEXT NOT NULL,
  PRIMARY KEY (meal_plan_id, list_index)
)
|]

data DbMealPlanNoteT f
  = DbMealPlanNote
      { mMealPlanId :: C f Int32
      , mListIndex  :: C f Int32
      , mNote       :: C f Text
      }
  deriving (Generic)

instance Beamable DbMealPlanNoteT

instance Table DbMealPlanNoteT where
  data PrimaryKey DbMealPlanNoteT f = DbMealPlanNoteId (C f Int32) (C f Int32) deriving (Generic)
  primaryKey = DbMealPlanNoteId <$> getField @"mMealPlanId" <*> getField @"mListIndex"

instance Beamable (PrimaryKey DbMealPlanNoteT)
