{-# OPTIONS_GHC -fno-warn-orphans #-}
module KonBoard.Db.Orphans
    (
    ) where

import           Database.Beam.Sqlite (SqliteM (..))

import           KonBoard.Base        (MonadThrow)

deriving instance MonadThrow SqliteM
