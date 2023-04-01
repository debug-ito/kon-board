module KonBoard.Query
    ( Query(..)
    ) where

import           KonBoard.Base (Generic, Text)

data Query
  = Query
      { query  :: Text
      , count  :: Int
      , offset :: Int
      }
  deriving (Eq, Generic, Ord, Show)
