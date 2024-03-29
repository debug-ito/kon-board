{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
module KonBoard.Db
    ( Conn
    , newSqliteConn
    , close
    , initDb
    , recipeStoreDb
    , mealPlanStoreDb
    ) where

import qualified Data.String.Interpolate                  as I
import qualified Data.Text                                as T
import qualified Data.Text.Read                           as TRead
import           Database.Beam                            (Beamable, C, Database, DatabaseSettings,
                                                           HasQBuilder, Identity, MonadBeam,
                                                           PrimaryKey, QExpr, Table (..),
                                                           TableEntity, pk, (&&.), (==.), (>.),
                                                           (||.))
import qualified Database.Beam                            as Beam
import           Database.Beam.Backend                    (BeamSqlBackend)
import           Database.Beam.Backend.SQL.BeamExtensions (MonadBeamInsertReturning (..))
import           Database.Beam.Query.CustomSQL            (customExpr_)
import           Database.Beam.Sqlite                     (Sqlite, SqliteM, runBeamSqlite)
import qualified Database.SQLite.Simple                   as SQLite

import           KonBoard.Base                            (ByteString, Day, Generic, HasField (..),
                                                           Int32, Int8, IsString, MonadIO (..),
                                                           MonadThrow, Text, UTCTime, Word32,
                                                           fromGregorian, throwString, toGregorian,
                                                           toList, void)
import           KonBoard.Db.Orphans                      ()
import           KonBoard.MealPlan                        (MealPhase (..), MealPlan (..),
                                                           MealPlanStore (..))
import           KonBoard.Query                           (Answer (..), QTerms (..), parseQTerms)
import qualified KonBoard.Query                           as KQuery (Query (..))
import           KonBoard.Recipe                          (Id, IngDesc (..), Ingredient (..),
                                                           Recipe, RecipeStore (..),
                                                           RecipeStored (..), Ref (..), parseRecipe)


sqlCreateDbRecipeT :: SQLite.Query
sqlCreateDbRecipeT = [I.i|
CREATE TABLE IF NOT EXISTS recipes (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  name TEXT NOT NULL UNIQUE,
  search_text TEXT NOT NULL,
  raw_yaml BLOB NOT NULL,
  created_at TEXT NOT NULL DEFAULT (datetime('now'))
)|]

data DbRecipeT f
  = DbRecipe
      { rId         :: C f Int32
      , rName       :: C f Text
      , rSearchText :: C f Text
      , rRawYaml    :: C f ByteString
      , rCreatedAt  :: C f UTCTime
      }
  deriving (Generic)

instance Beamable DbRecipeT

instance Table DbRecipeT where
  data PrimaryKey DbRecipeT f = DbRecipeId (C f Int32) deriving (Generic)
  primaryKey = DbRecipeId . getField @"rId"

instance Beamable (PrimaryKey DbRecipeT)

data Db f
  = Db
      { dbRecipes         :: f (TableEntity DbRecipeT)
      , dbMealPlanHeaders :: f (TableEntity DbMealPlanHeaderT)
      , dbMealPlanRecipes :: f (TableEntity DbMealPlanRecipeT)
      , dbMealPlanNotes   :: f (TableEntity DbMealPlanNoteT)
      }
  deriving (Generic)

instance Database be Db

data Conn
  = Conn SQLite.Connection

newSqliteConn :: MonadIO m => FilePath -> m Conn
newSqliteConn f = liftIO $ do
  sqC <- SQLite.open f
  SQLite.execute_ sqC "PRAGMA foreign_keys = true;\n"
  return $ Conn sqC

close :: MonadIO m => Conn -> m ()
close (Conn c) = liftIO $ SQLite.close c

runBeamSqliteTx :: Conn -> SqliteM a -> IO a
runBeamSqliteTx (Conn c) action = SQLite.withTransaction c $ runBeamSqlite c action

recipeStoreDb :: (MonadIO m, MonadThrow m) => Conn -> RecipeStore m
recipeStoreDb c =
  RecipeStore
  { addRecipe = \r -> liftIO $ runBeamSqliteTx c $ fmap toRecipeId $ addDbRecipe $ toDbRecipe r
  , updateRecipe = \r -> do
      dbR <- liftIO $ toDbRecipeStored r
      liftIO $ runBeamSqliteTx c $ updateDbRecipe dbR
  , getRecipeById = \i -> do
      dbrId <- fromRecipeId i
      traverse fromDbRecipe =<< (liftIO $ runBeamSqliteTx c $ getDbRecipeById dbrId)
  , getRecipeByName = \n -> traverse fromDbRecipe =<< (liftIO $ runBeamSqliteTx c $ getDbRecipeByName n)
  , getRecipesByQuery = \q -> do
      qTerms <- either throwString return $ parseQTerms $ getField @"query" q
      dbRs <- liftIO $ runBeamSqliteTx c $ getDbRecipesByQuery qTerms (getField @"count" q) (getField @"offset" q)
      traverse fromDbRecipe dbRs
  }

-- | You should call this function once in the process before doing anything on the database.
initDb :: MonadIO m => Conn -> m ()
initDb (Conn c) =
  liftIO $ mapM_ (SQLite.execute_ c)
  [ sqlCreateDbRecipeT
  , sqlCreateDbMealPlanHeaderT
  , sqlCreateDbMealPlanRecipeT
  , sqlCreateDbMealPlanNoteT
  ]

type Backend = Sqlite

dbSettings :: DatabaseSettings be Db
dbSettings = Beam.defaultDbSettings

makeSearchText :: Recipe -> Text
makeSearchText r = T.intercalate "\n" elements
  where
    elements = [getField @"name" r, getField @"description" r] ++ foodItems ++ refs
    foodItems = selectFoodItem =<< getField @"ingredients" r
    selectFoodItem (IngSingle (Ingredient f _)) = [f]
    selectFoodItem (IngGroup _ ings)            = map (\(Ingredient f _) -> f) ings
    refs = refTexts =<< getField @"references" r
    refTexts ref =
      case ref of
        RefSource t -> [t]
        RefUrl u ma -> [u] ++ toList ma

toDbRecipe :: Recipe -> DbRecipeT (QExpr Backend s)
toDbRecipe r =
  DbRecipe { rId = Beam.default_
           , rName = Beam.val_ $ getField @"name" r
           , rSearchText = Beam.val_ $ makeSearchText r
           , rRawYaml = Beam.val_ $ getField @"rawYaml" r
           , rCreatedAt = Beam.default_
           }

toDbRecipeStored :: (MonadThrow m) => RecipeStored -> m (DbRecipeT Identity)
toDbRecipeStored rs = do
  ri <- fromRecipeId $ getField @"id" rs
  return $ DbRecipe { rId = ri
                    , rName = getField @"name" r
                    , rSearchText = makeSearchText r
                    , rRawYaml = getField @"rawYaml" r
                    , rCreatedAt = getField @"createdAt" rs
                    }
  where
    r = getField @"recipe" rs

fromDbRecipe :: (MonadThrow m) => DbRecipeT Identity -> m RecipeStored
fromDbRecipe dbR = do
  r <- either throwString return $ parseRecipe $ getField @"rRawYaml" dbR
  return $ RecipeStored { recipe = r
                        , id = toRecipeId $ getField @"rId" dbR
                        , createdAt = getField @"rCreatedAt" dbR
                        }

toRecipeId :: Int32 -> Id
toRecipeId = T.pack . show

fromRecipeId :: (MonadThrow m) => Id -> m Int32
fromRecipeId ri = either throwString return $ fmap fst $ TRead.decimal ri

addDbRecipe :: (MonadBeamInsertReturning Backend m, MonadThrow m) => DbRecipeT (QExpr Backend s) -> m Int32
addDbRecipe r = fmap (getField @"rId") $ takeFirst "get no insert result" =<< (runInsertReturningList $ Beam.insertOnly table cols vals)
  where
    table = getField @"dbRecipes" dbSettings
    -- SQLite doesn't support "DEFAULT" for column expression, so we need to specify inserted columns explicitly.
    cols t = (rName t, rSearchText t, rRawYaml t)
    vals = Beam.insertData [(rName r, rSearchText r, rRawYaml r)]

takeFirst :: MonadThrow m => String -> [a] -> m a
takeFirst err []  = throwString err
takeFirst _ (x:_) = return x

updateDbRecipe :: (MonadBeam Backend m) => DbRecipeT Identity -> m ()
updateDbRecipe r = Beam.runUpdate $ Beam.save (getField @"dbRecipes" dbSettings) r

getDbRecipeById :: (MonadBeam Backend m) => Int32 -> m (Maybe (DbRecipeT Identity))
getDbRecipeById recipeId = Beam.runSelectReturningOne $ Beam.lookup_ (getField @"dbRecipes" dbSettings) (DbRecipeId recipeId)

getDbRecipeByName :: (MonadBeam Backend m) => Text -> m (Maybe (DbRecipeT Identity))
getDbRecipeByName recipeName = Beam.runSelectReturningOne $ Beam.select query
  where
    query = do
      r <- Beam.all_ $ getField @"dbRecipes" dbSettings
      Beam.guard_ $ getField @"rName" r ==. Beam.val_ recipeName
      return r

getDbRecipesByQuery :: (MonadBeam Backend m, MonadThrow m) => QTerms -> Word -> Word -> m (Answer (DbRecipeT Identity))
getDbRecipesByQuery (QTerms qTerms) count ofs = do
  (tCount :: Word32) <- takeFirst "Something is terribly wrong. Got no row from count() function."
                        =<< (Beam.runSelectReturningList $ Beam.select $ selectTotalCount)
  if tCount == 0
    then return $ Answer { items = [], offset = ofs, totalCount = fromIntegral tCount }
    else do
    rs <- Beam.runSelectReturningList $ Beam.select $ modifyQuery $ selectRecipes
    return $ Answer { items = rs
                    , offset = ofs
                    , totalCount = fromIntegral tCount
                    }
  where
    selectRecipes :: Beam.Q Backend Db s (DbRecipeT (QExpr Backend s))
    selectRecipes = do
      r <- Beam.all_ $ getField @"dbRecipes" dbSettings
      mapM_ (containsTerm r) qTerms
      return r
    selectTotalCount = do
      void $ selectRecipes
      return $ Beam.countAll_
    containsTerm r t = Beam.guard_ $ likeEscaped (getField @"rSearchText" r) t
    modifyQuery q = Beam.limit_ (fromIntegral count) $ Beam.offset_ (fromIntegral ofs) $ Beam.orderBy_ order q
    order r = Beam.asc_ $ getField @"rName" r

likeEscaped :: Beam.QGenExpr c Backend s Text -> Text -> Beam.QGenExpr c Backend s Bool
likeEscaped target rawPattern = likeEscapedOp target $ Beam.val_ $ addWildCards $ escape "_" $ escape "%" $ escape escapeChar rawPattern
  where
    escapeChar :: (Monoid a, IsString a) => a
    escapeChar = "\\"
    escape c t = T.replace c (escapeChar <> c) t
    addWildCards t = "%" <> t <> "%"
    likeEscapedOp :: Beam.QGenExpr c Backend s Text -> Beam.QGenExpr c Backend s Text -> Beam.QGenExpr c Backend s Bool
    likeEscapedOp = customExpr_ likeEscapedStr
    likeEscapedStr :: (Monoid b, IsString b) => b -> b -> b
    likeEscapedStr l r = l <> " LIKE " <> r <> " ESCAPE '" <> escapeChar <> "'"

----------------------------------------------------------------

sqlCreateDbMealPlanHeaderT :: SQLite.Query
sqlCreateDbMealPlanHeaderT = [I.i|
CREATE TABLE IF NOT EXISTS meal_plan_headers (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  year INTEGER NOT NULL,
  month INTEGER NOT NULL,
  day_of_month INTEGER NOT NULL,
  phase TEXT NOT NULL,
  UNIQUE (year, month, day_of_month, phase)
)
|]

type DbMealPhase = Text

toDbMealPhase :: MealPhase -> DbMealPhase
toDbMealPhase mp =
  case mp of
    Breakfast   -> "0"
    Lunch       -> "1"
    Dinner      -> "2"
    MealOther s -> "@" <> s

fromDbMealPhase :: MonadThrow m => DbMealPhase -> m MealPhase
fromDbMealPhase mp =
  case mp of
    "0" -> return Breakfast
    "1" -> return Lunch
    "2" -> return Dinner
    _ -> maybe (throwString ("unknown MealPhase string: " <> T.unpack mp)) (return . MealOther) $ T.stripPrefix "@" mp

data DbMealPlanHeaderT f
  = DbMealPlanHeader
      { mId         :: C f Int32
      , mYear       :: C f Int32
      , mMonth      :: C f Int8
      , mDayOfMonth :: C f Int8
      , mPhase      :: C f DbMealPhase
      }
  deriving (Generic)

instance Beamable DbMealPlanHeaderT

instance Table DbMealPlanHeaderT where
  data PrimaryKey DbMealPlanHeaderT f = DbMealPlanHeaderId (C f Int32) deriving (Generic)
  primaryKey = DbMealPlanHeaderId . getField @"mId"

instance Beamable (PrimaryKey DbMealPlanHeaderT)

toGregorianDb :: Day -> (Int32, Int8, Int8)
toGregorianDb d = (fromIntegral year, fromIntegral month, fromIntegral dom)
  where
    (year, month, dom) = toGregorian d

fromGregorianDb :: Int32 -> Int8 -> Int8 -> Day
fromGregorianDb y m d = fromGregorian (fromIntegral y) (fromIntegral m) (fromIntegral d)

isNewerThan :: (QExpr Backend s Int32, QExpr Backend s Int8, QExpr Backend s Int8) -> (QExpr Backend s Int32, QExpr Backend s Int8, QExpr Backend s Int8) -> QExpr Backend s Bool
isNewerThan (aY, aM, aD) (bY, bM, bD) =
  (aY >. bY) ||. ((aY ==. bY) &&. ((aM >. bM) ||. ((aM ==. bM) &&. (aD >. bD))))

getMealPlanHeader :: (MonadBeam Backend m) => Day -> DbMealPhase -> m (Maybe (DbMealPlanHeaderT Identity))
getMealPlanHeader d p = Beam.runSelectReturningOne $ Beam.select $ query
  where
    (year, month, dom) = toGregorianDb d
    query = do
      h <- Beam.all_ $ dbMealPlanHeaders dbSettings
      Beam.guard_ $ mYear h ==. Beam.val_ year
      Beam.guard_ $ mMonth h ==. Beam.val_ month
      Beam.guard_ $ mDayOfMonth h ==. Beam.val_ dom
      Beam.guard_ $ mPhase h ==. Beam.val_ p
      return h

getMealPlanHeadersByDayRange :: (MonadBeam Backend m) => Day -> Day -> m [DbMealPlanHeaderT Identity]
getMealPlanHeadersByDayRange startDay endDay = Beam.runSelectReturningList $ Beam.select $ Beam.orderBy_ order $ query
  where
    (startYear, startMonth, startDom) = toGregorianDb startDay
    startDayQ = (Beam.val_ startYear, Beam.val_ startMonth, Beam.val_ startDom)
    (endYear, endMonth, endDom) = toGregorianDb endDay
    endDayQ = (Beam.val_ endYear, Beam.val_ endMonth, Beam.val_ endDom)
    query = do
      header <- Beam.all_ $ dbMealPlanHeaders dbSettings
      let headerDayQ = (mYear header, mMonth header, mDayOfMonth header)
      Beam.guard_ ((endDayQ `isNewerThan` headerDayQ) &&. (Beam.not_ $ startDayQ `isNewerThan` headerDayQ))
      return header
    order h = ( Beam.asc_ $ getField @"mYear" h
              , Beam.asc_ $ getField @"mMonth" h
              , Beam.asc_ $ getField @"mDayOfMonth" h
              , Beam.asc_ $ getField @"mPhase" h
              )

ensureMealPlanHeader :: (MonadBeamInsertReturning Backend m, MonadThrow m) => Day -> DbMealPhase -> m (DbMealPlanHeaderT Identity)
ensureMealPlanHeader d p = do
  mHeader <- getMealPlanHeader d p
  case mHeader of
    Just h -> return h
    Nothing -> takeFirst "get no insert result" =<< (runInsertReturningList $ Beam.insertOnly (dbMealPlanHeaders dbSettings) cols $ Beam.insertData vals)
  where
    (year, month, dom) = toGregorianDb d
    cols t = (mYear t, mMonth t, mDayOfMonth t, mPhase t)
    vals :: [(QExpr Backend s Int32, QExpr Backend s Int8, QExpr Backend s Int8, QExpr Backend s Text)] -- we need this signature to compile.
    vals = [(Beam.val_ year, Beam.val_ month, Beam.val_ dom, Beam.val_ p)]

----------------------------------------------------------------

sqlCreateDbMealPlanRecipeT :: SQLite.Query
sqlCreateDbMealPlanRecipeT = [I.i|
CREATE TABLE IF NOT EXISTS meal_plan_recipes (
  meal_plan__id INTEGER NOT NULL REFERENCES meal_plan_headers (id) ON DELETE CASCADE ON UPDATE CASCADE,
  list_index INTEGER NOT NULL,
  recipe__id INTEGER NOT NULL REFERENCES recipes (id) ON DELETE CASCADE ON UPDATE CASCADE,
  PRIMARY KEY (meal_plan__id, list_index)
)
|]

data DbMealPlanRecipeT f
  = DbMealPlanRecipe
      { mMealPlan  :: PrimaryKey DbMealPlanHeaderT f
      , mListIndex :: C f Int32
      , mRecipe    :: PrimaryKey DbRecipeT f
      }
  deriving (Generic)

instance Beamable DbMealPlanRecipeT

instance Table DbMealPlanRecipeT where
  data PrimaryKey DbMealPlanRecipeT f = DbMealPlanRecipeId (PrimaryKey DbMealPlanHeaderT f) (C f Int32) deriving (Generic)
  primaryKey = DbMealPlanRecipeId <$> getField @"mMealPlan" <*> getField @"mListIndex"

instance Beamable (PrimaryKey DbMealPlanRecipeT) where

deleteMealPlanRecipes :: (MonadBeam Backend m) => PrimaryKey DbMealPlanHeaderT Identity -> m ()
deleteMealPlanRecipes headerId = Beam.runDelete $ Beam.delete (dbMealPlanRecipes dbSettings) $ \t -> condition t
  where
    condition :: forall s. (forall s'. DbMealPlanRecipeT (QExpr Backend s')) -> QExpr Backend s Bool
    condition t = getField @"mMealPlan" t ==. Beam.val_ headerId

insertMealPlanRecipes :: (MonadBeam Backend m) => PrimaryKey DbMealPlanHeaderT Identity -> [PrimaryKey DbRecipeT Identity] -> m ()
insertMealPlanRecipes headerId recipeIds = Beam.runInsert $ Beam.insert (dbMealPlanRecipes dbSettings) $ Beam.insertValues $ map toRecipeRef $ zip [0..] recipeIds
  where
    toRecipeRef (index, recipeId) = DbMealPlanRecipe { mMealPlan = headerId, mListIndex = index, mRecipe = recipeId }

getMealPlanRecipes :: (MonadBeam Backend m) => PrimaryKey DbMealPlanHeaderT Identity -> m [DbRecipeT Identity]
getMealPlanRecipes hId = fmap extractRecipes $ Beam.runSelectReturningList $ Beam.select $ Beam.orderBy_ order $ query
  where
    query = do
      mpR <- Beam.all_ (dbMealPlanRecipes dbSettings)
      Beam.guard_ (getField @"mMealPlan" mpR ==. Beam.val_ hId)
      r <- Beam.join_ (getField @"dbRecipes" dbSettings) (Beam.references_ $ getField @"mRecipe" mpR)
      return (getField @"mListIndex" mpR, r)
    order (listIndex, _) = Beam.asc_ listIndex
    extractRecipes = map (\(_, r) -> r)


----------------------------------------------------------------

sqlCreateDbMealPlanNoteT :: SQLite.Query
sqlCreateDbMealPlanNoteT = [I.i|
CREATE TABLE IF NOT EXISTS meal_plan_notes (
  meal_plan__id INTEGER NOT NULL REFERENCES meal_plan_headers (id) ON DELETE CASCADE ON UPDATE CASCADE,
  list_index INTEGER NOT NULL,
  note TEXT NOT NULL,
  PRIMARY KEY (meal_plan__id, list_index)
)
|]

data DbMealPlanNoteT f
  = DbMealPlanNote
      { mMealPlan  :: PrimaryKey DbMealPlanHeaderT f
      , mListIndex :: C f Int32
      , mNote      :: C f Text
      }
  deriving (Generic)

instance Beamable DbMealPlanNoteT

instance Table DbMealPlanNoteT where
  data PrimaryKey DbMealPlanNoteT f = DbMealPlanNoteId (PrimaryKey DbMealPlanHeaderT f) (C f Int32) deriving (Generic)
  primaryKey = DbMealPlanNoteId <$> getField @"mMealPlan" <*> getField @"mListIndex"

instance Beamable (PrimaryKey DbMealPlanNoteT)

deleteMealPlanNotes :: (MonadBeam Backend m) => PrimaryKey DbMealPlanHeaderT Identity -> m ()
deleteMealPlanNotes headerId = Beam.runDelete $ Beam.delete (dbMealPlanNotes dbSettings) $ \t -> condition t
  where
    condition :: forall s. (forall s'. DbMealPlanNoteT (QExpr Backend s')) -> QExpr Backend s Bool
    condition t = getField @"mMealPlan" t ==. Beam.val_ headerId

insertMealPlanNotes :: (MonadBeam Backend m) => PrimaryKey DbMealPlanHeaderT Identity -> [Text] -> m ()
insertMealPlanNotes headerId ns = Beam.runInsert $ Beam.insert (dbMealPlanNotes dbSettings) $ Beam.insertValues $ map toNoteTableEntry $ zip [0..] ns
  where
    toNoteTableEntry (index, note) = DbMealPlanNote { mMealPlan = headerId, mListIndex = index, mNote = note }

getMealPlanNotes :: (MonadBeam Backend m) => PrimaryKey DbMealPlanHeaderT Identity -> m [Text]
getMealPlanNotes hId = fmap (map $ getField @"mNote") $ Beam.runSelectReturningList $ Beam.select $ Beam.orderBy_ order $ query
  where
    query = do
      n <- Beam.all_ (dbMealPlanNotes dbSettings)
      Beam.guard_ (getField @"mMealPlan" n ==. Beam.val_ hId)
      return n
    order n = Beam.asc_ $ getField @"mListIndex" n

----------------------------------------------------------------

putDbMealPlans :: (MonadBeamInsertReturning Backend m, MonadThrow m) => Day -> DbMealPhase -> [PrimaryKey DbRecipeT Identity] -> [Text] -> m ()
putDbMealPlans d p rs ns = do
  header <- ensureMealPlanHeader d p
  let headerId = pk header
  deleteMealPlanRecipes headerId
  deleteMealPlanNotes headerId
  insertMealPlanRecipes headerId rs
  insertMealPlanNotes headerId ns

getDbMealPlans :: (MonadBeam Backend m) => Day -> Day -> m [(DbMealPlanHeaderT Identity, [DbRecipeT Identity], [Text])]
getDbMealPlans startDay endDay = traverse getCompleteMealPlan =<< getMealPlanHeadersByDayRange startDay endDay
  where
    getCompleteMealPlan h = (,,) h <$> getMealPlanRecipes (pk h) <*> getMealPlanNotes (pk h)

toMealPlan :: (MonadThrow m) => (DbMealPlanHeaderT Identity, [DbRecipeT Identity], [Text]) -> m (MealPlan RecipeStored)
toMealPlan (header, rs, ns) = do
  p <- fromDbMealPhase $ getField @"mPhase" header
  rStored <- traverse fromDbRecipe rs
  let mp = MealPlan
           { day = fromGregorianDb (getField @"mYear" header) (getField @"mMonth" header) (getField @"mDayOfMonth" header)
           , phase = p
           , recipes = rStored
           , notes = ns
           }
  return mp

mealPlanStoreDb :: (MonadThrow m, MonadIO m) => Conn -> MealPlanStore m
mealPlanStoreDb c = MealPlanStore { putMealPlan = putMpImpl, getMealPlans = getMpImpl }
  where
    putMpImpl mp = do
      rIds <- fmap (map DbRecipeId) $ traverse fromRecipeId $ getField @"recipes" mp
      liftIO $ runBeamSqliteTx c $ putDbMealPlans (getField @"day" mp) (toDbMealPhase $ getField @"phase" mp) rIds (getField @"notes" mp)
    getMpImpl startDay endDay = liftIO $ runBeamSqliteTx c $ traverse toMealPlan =<< (getDbMealPlans startDay endDay)
