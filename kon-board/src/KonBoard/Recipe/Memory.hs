module KonBoard.Recipe.Memory
    ( recipeStoreMemory
    ) where

import qualified Crypto.Hash.MD5        as MD5
import qualified Data.ByteString.Base16 as Base16
import qualified Data.HashMap.Strict    as HM
import qualified Data.Text              as T
import           Data.Text.Encoding     (decodeUtf8, encodeUtf8)

import           KonBoard.Base          (ByteString, HasField (..), HashMap, IORef, MonadIO (..),
                                         MonadLogger, MonadThrow, Monoid (..), Semigroup (..),
                                         newIORef, throwString, when)
import           KonBoard.Recipe        (Id, Name, Recipe (..), RecipeStore (..), RecipeStored (..))

recipeStoreMemory :: (MonadIO m1, MonadIO m2) => m1 (RecipeStore m2)
recipeStoreMemory = do
  refStore <- liftIO $ (newIORef mempty :: IO (IORef RecipeStoreMemory))
  return $ RecipeStore { insertRecipe = undefined -- TODO
                       , updateRecipe = undefined -- TODO
                       , getRecipeById = undefined -- TODO
                       , getRecipeByName = undefined -- TODO
                       }


-- | On-memory Recipe store.
data RecipeStoreMemory
  = RecipeStoreMemory
      { fromId   :: HashMap Id RecipeStored
      , fromName :: HashMap Name RecipeStored
      }
  deriving (Eq, Show)

instance Semigroup RecipeStoreMemory where
  a <> b = RecipeStoreMemory (fromId a <> fromId b) (fromName a <> fromName b)

instance Monoid RecipeStoreMemory where
  mappend = (<>)
  mempty = RecipeStoreMemory mempty mempty

-- | Unsafe means that this function doesn't check the association between the ID and Name.
insertUnsafe :: RecipeStored -> RecipeStoreMemory -> RecipeStoreMemory
insertUnsafe r rs = updated
  where
    updated = rs { fromId = HM.insert rId r $ fromId rs
                 , fromName = HM.insert rName r $ fromName rs
                 }
    rId = getField @"id" r
    rName = getField @"name" $ getField @"recipe" r

insertRecipePure :: MonadThrow m => Recipe -> RecipeStoreMemory -> m (RecipeStoreMemory, Id)
insertRecipePure r rs = do
  when (HM.member rname $ fromName rs) $ do
    throwString ("Conflict of recipe name: " <> T.unpack rname)
  when (HM.member rid $ fromId rs) $ do
    throwString ("Conflict of recipe ID (" <> T.unpack rid <> ") for name: " <> T.unpack rname)
  return (insertUnsafe (RecipeStored rid r) rs, rid)
  where
    rname = getField @"name" r
    rid = makeId rname

makeId :: Name -> Id
makeId = decodeUtf8 . Base16.encode . MD5.hash . encodeUtf8

updateRecipePure :: MonadThrow m => RecipeStored -> RecipeStoreMemory -> m RecipeStoreMemory
updateRecipePure updated rs = do
  when (rId /= rIdFromName) $ do
    throwString ("Mismatch of ID. RecipeStoreMemory doesn't support changing the name. New name was: " <> T.unpack rName)
  return $ insertUnsafe updated rs
  where
    rId = getField @"id" updated
    rName = getField @"name" $ getField @"recipe" updated
    rIdFromName = makeId rName

getRecipeByIdPure :: Id -> RecipeStoreMemory -> Maybe RecipeStored
getRecipeByIdPure i rs = HM.lookup i $ fromId rs

getRecipeByNamePure :: Name -> RecipeStoreMemory -> Maybe RecipeStored
getRecipeByNamePure n rs = HM.lookup n $ fromName rs


---- -- | URL-fiendly ID for a recipe
---- type ID = Text
----
---- -- | Consise summary of a 'Recipe'.
---- data RecipeSummary
----   = RecipeSummary
----       { rsID   :: ID
----       , rsName :: Name
----       }
----   deriving (Eq, Generic, Ord, Show)
----
---- data RecipeWithID
----   = RecipeWithID
----       { rwID     :: ID
----       , rwRecipe :: Recipe
----       }
----   deriving (Eq, Ord, Show)
----
---- withIDToSummary :: RecipeWithID -> RecipeSummary
---- withIDToSummary rw = RecipeSummary (rwID rw) (recipeName $ rwRecipe rw)
----
---- -- | Handle for the recipe storage.
---- data RecipeStore
----   = RecipeStore
----       { fromID   :: HashMap ID RecipeWithID
----       , fromName :: HashMap Name RecipeWithID
----       }
----
---- emptyStore :: RecipeStore
---- emptyStore = RecipeStore HM.empty HM.empty
----
---- makeID :: Name -> ID
---- makeID = decodeUtf8 . Base16.encode . MD5.hash . encodeUtf8
----
---- -- | Exception about 'RecipeStore'.
---- data RecipeStoreException
----   = ConflictingID ID
----   -- ^ ID conflict in the store.
----   | ConflictingName Name
----   -- ^ Name conflict in the store.
----   | IDNotFound ID
----   -- ^ ID is not found in the store.
----   | NameNotFound Name
----   -- ^ Recipe name is not found in the store.
----   deriving (Eq, Ord, Show)
----
---- instance Exception RecipeStoreException
----
---- addRecipe :: Recipe -> RecipeStore -> Either RecipeStoreException RecipeStore
---- addRecipe r rs = do
----   when (HM.member rname $ fromName rs) $ do
----     Left $ ConflictingName rname
----   when (HM.member rid $ fromID rs) $ do
----     Left $ ConflictingID rid
----   Right $ rs { fromID = HM.insert rid rWithID $ fromID rs,
----                fromName = HM.insert rname rWithID $ fromName rs
----              }
----   where
----     rname = recipeName r
----     rid = makeID rname
----     rWithID = RecipeWithID rid r
----
---- toRecipeSummary :: ID -> Recipe -> RecipeSummary
---- toRecipeSummary rid r = RecipeSummary rid $ recipeName r
----
---- loadRecipeByName :: RecipeStore -> Name -> IO (Maybe RecipeSummary)
---- loadRecipeByName store name = return $ fmap withIDToSummary $ HM.lookup name $ fromName store
----
---- -- | Same as 'loadRecipeByName' except that 'Nothing' is converted to
---- -- 'NameNotFound' exception.
---- loadRecipeByName' :: RecipeStore -> Name -> IO RecipeSummary
---- loadRecipeByName' rs n = maybe (throwIO $ NameNotFound n) return =<< loadRecipeByName rs n
----
---- loadRecipe :: RecipeStore -> ID -> IO Recipe
---- loadRecipe store rid = maybe (throwIO $ IDNotFound rid) return $ fmap rwRecipe $ HM.lookup rid $ fromID store
----
---- loadRecipeSummary :: RecipeStore -> ID -> IO RecipeSummary
---- loadRecipeSummary store rid = fmap (toRecipeSummary rid) $ loadRecipe store rid
----
---- toIO :: Exception e => Either e a -> IO a
---- toIO (Right a) = return a
---- toIO (Left e)  = throwIO e
----
---- -- | Open YAML files and make a 'RecipeStore'.
---- openYAMLs :: (MonadLogger m, MonadIO m) => [FilePath] -> m RecipeStore
---- openYAMLs files = do
----   ref_store <- liftIO $ newIORef emptyStore
----   forM_ files $ \file -> do
----     logInfoN ("Read recipe YAML: " <> pack file)
----     rs <- liftIO $ readRecipes file
----     liftIO $ mapM_ (addR ref_store) rs
----   liftIO $ readIORef ref_store
----   where
----     readRecipes file = (toIO . loadYAML) =<< BS.readFile file
----     addR ref_store r = do
----       store <- readIORef ref_store
----       added <- toIO $ addRecipe r store
----       writeIORef ref_store $ added

