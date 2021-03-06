{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
-- |
-- Module: KonBoard.Recipe.Store
-- Description: Storage service for Recipes
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module KonBoard.Recipe.Store
  ( -- * RecipeSummary
    RecipeSummary(..),
    ID,
    toRecipeSummary,
    -- * RecipeStore
    RecipeStore,
    openYAMLs,
    loadRecipeByName,
    loadRecipeByName',
    loadRecipe,
    loadRecipeSummary,
    RecipeStoreException(..)
  ) where

import Control.Exception.Safe (Exception, throwIO)
import Control.Monad (when, forM_, mapM_)
import Control.Monad.Logger (MonadLogger, logInfoN)
import Control.Monad.Trans (MonadIO(..))
import qualified Crypto.Hash.MD5 as MD5
import Data.Aeson (FromJSON(..), ToJSON(..))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import Data.Char (toLower)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Traversable (Traversable(..))
import GHC.Generics (Generic)

import KonBoard.Recipe
  ( Recipe(recipeName),
    Name,
    loadYAML
  )

-- | URL-fiendly ID for a recipe
type ID = Text

-- | Consise summary of a 'Recipe'.
data RecipeSummary =
  RecipeSummary
  { rsID :: ID,
    rsName :: Name
  }
  deriving (Show,Eq,Ord,Generic)

data RecipeWithID =
  RecipeWithID
  { rwID :: ID,
    rwRecipe :: Recipe
  }
  deriving (Show,Eq,Ord)

withIDToSummary :: RecipeWithID -> RecipeSummary
withIDToSummary rw = RecipeSummary (rwID rw) (recipeName $ rwRecipe rw)

-- | Handle for the recipe storage.
data RecipeStore =
  RecipeStore
  { fromID :: HashMap ID RecipeWithID,
    fromName :: HashMap Name RecipeWithID
  }

emptyStore :: RecipeStore
emptyStore = RecipeStore HM.empty HM.empty

makeID :: Name -> ID
makeID = decodeUtf8 . Base16.encode . MD5.hash . encodeUtf8

-- | Exception about 'RecipeStore'.
data RecipeStoreException =
    ConflictingID ID -- ^ ID conflict in the store.
  | ConflictingName Name -- ^ Name conflict in the store.
  | IDNotFound ID -- ^ ID is not found in the store.
  | NameNotFound Name -- ^ Recipe name is not found in the store.
  deriving (Show,Eq,Ord)

instance Exception RecipeStoreException

addRecipe :: Recipe -> RecipeStore -> Either RecipeStoreException RecipeStore
addRecipe r rs = do
  when (HM.member rname $ fromName rs) $ do
    Left $ ConflictingName rname
  when (HM.member rid $ fromID rs) $ do
    Left $ ConflictingID rid
  Right $ rs { fromID = HM.insert rid rWithID $ fromID rs,
               fromName = HM.insert rname rWithID $ fromName rs
             }
  where
    rname = recipeName r
    rid = makeID rname
    rWithID = RecipeWithID rid r

toRecipeSummary :: ID -> Recipe -> RecipeSummary
toRecipeSummary rid r = RecipeSummary rid $ recipeName r

loadRecipeByName :: RecipeStore -> Name -> IO (Maybe RecipeSummary)
loadRecipeByName store name = return $ fmap withIDToSummary $ HM.lookup name $ fromName store

-- | Same as 'loadRecipeByName' except that 'Nothing' is converted to
-- 'NameNotFound' exception.
loadRecipeByName' :: RecipeStore -> Name -> IO RecipeSummary
loadRecipeByName' rs n = maybe (throwIO $ NameNotFound n) return =<< loadRecipeByName rs n

loadRecipe :: RecipeStore -> ID -> IO Recipe
loadRecipe store rid = maybe (throwIO $ IDNotFound rid) return $ fmap rwRecipe $ HM.lookup rid $ fromID store

loadRecipeSummary :: RecipeStore -> ID -> IO RecipeSummary
loadRecipeSummary store rid = fmap (toRecipeSummary rid) $ loadRecipe store rid

toIO :: Exception e => Either e a -> IO a
toIO (Right a) = return a
toIO (Left e) = throwIO e

-- | Open YAML files and make a 'RecipeStore'.
openYAMLs :: (MonadLogger m, MonadIO m) => [FilePath] -> m RecipeStore
openYAMLs files = do
  ref_store <- liftIO $ newIORef emptyStore
  forM_ files $ \file -> do
    logInfoN ("Read recipe YAML: " <> pack file)
    rs <- liftIO $ readRecipes file
    liftIO $ mapM_ (addR ref_store) rs
  liftIO $ readIORef ref_store
  where
    readRecipes file = (toIO . loadYAML) =<< BS.readFile file
    addR ref_store r = do
      store <- readIORef ref_store
      added <- toIO $ addRecipe r store
      writeIORef ref_store $ added

