module KonBoard.Query
    ( Query(..)
    , Answer(..)
    ) where

import           KonBoard.Base (Generic, Text)

data Query
  = Query
      { query  :: Text
      , count  :: Word
      , offset :: Word
      }
  deriving (Eq, Generic, Ord, Show)

data Answer a
  = Answer
      { items   :: [a]
      , hasNext :: Bool
      }
  deriving (Eq, Foldable, Functor, Generic, Ord, Show, Traversable)
