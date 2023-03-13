module KonBoard.Recipe.Memory
    ( newRecipeStore
    ) where

import qualified Crypto.Hash.MD5        as MD5
import qualified Data.ByteString.Base16 as Base16
import qualified Data.HashMap.Strict    as HM
import qualified Data.Text              as T
import           Data.Text.Encoding     (decodeUtf8, encodeUtf8)

import           KonBoard.Base          (ByteString, HasField (..), HashMap, IORef, MonadIO (..),
                                         MonadLogger, MonadThrow, Monoid (..), Semigroup (..),
                                         UTCTime, getCurrentTime, newIORef, readIORef, throwString,
                                         when, writeIORef)
import           KonBoard.Recipe        (Id, Name, Recipe, RecipeStore (..), RecipeStored (..))

newRecipeStore :: (MonadIO m1, MonadIO m2, MonadThrow m2) => m1 (RecipeStore m2)
newRecipeStore = do
  refStore <- liftIO $ (newIORef mempty :: IO (IORef RecipeStoreMemory))
  let addImpl r = do
        rs <- liftIO $ readIORef refStore
        createTime <- liftIO $ getCurrentTime
        (rs', i) <- addRecipePure r createTime rs
        liftIO $ writeIORef refStore rs'
        return i
      updateImpl r = do
        rs <- liftIO $ readIORef refStore
        rs' <- updateRecipePure r rs
        liftIO $ writeIORef refStore rs'
        return ()
      getByIdImpl i = do
        rs <- liftIO $ readIORef refStore
        return $ getRecipeByIdPure i rs
      getByNameImpl n = do
        rs <- liftIO $ readIORef refStore
        return $ getRecipeByNamePure n rs
  return $ RecipeStore { addRecipe = addImpl
                       , updateRecipe = updateImpl
                       , getRecipeById = getByIdImpl
                       , getRecipeByName = getByNameImpl
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
addUnsafe :: RecipeStored -> RecipeStoreMemory -> RecipeStoreMemory
addUnsafe r rs = updated
  where
    updated = rs { fromId = HM.insert rId r $ fromId rs
                 , fromName = HM.insert rName r $ fromName rs
                 }
    rId = getField @"id" r
    rName = getField @"name" $ getField @"recipe" r

addRecipePure :: MonadThrow m => Recipe -> UTCTime -> RecipeStoreMemory -> m (RecipeStoreMemory, Id)
addRecipePure r createTime rs = do
  when (HM.member rname $ fromName rs) $ do
    throwString ("Conflict of recipe name: " <> T.unpack rname)
  when (HM.member rid $ fromId rs) $ do
    throwString ("Conflict of recipe ID (" <> T.unpack rid <> ") for name: " <> T.unpack rname)
  return (addUnsafe (RecipeStored r rid createTime) rs, rid)
  where
    rname = getField @"name" r
    rid = makeId rname

makeId :: Name -> Id
makeId = decodeUtf8 . Base16.encode . MD5.hash . encodeUtf8

updateRecipePure :: MonadThrow m => RecipeStored -> RecipeStoreMemory -> m RecipeStoreMemory
updateRecipePure updated rs = do
  when (rId /= rIdFromName) $ do
    throwString ("Mismatch of ID. RecipeStoreMemory doesn't support changing the name. New name was: " <> T.unpack rName)
  return $ addUnsafe updated rs
  where
    rId = getField @"id" updated
    rName = getField @"name" $ getField @"recipe" updated
    rIdFromName = makeId rName

getRecipeByIdPure :: Id -> RecipeStoreMemory -> Maybe RecipeStored
getRecipeByIdPure i rs = HM.lookup i $ fromId rs

getRecipeByNamePure :: Name -> RecipeStoreMemory -> Maybe RecipeStored
getRecipeByNamePure n rs = HM.lookup n $ fromName rs
