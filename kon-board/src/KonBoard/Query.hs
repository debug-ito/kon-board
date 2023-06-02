module KonBoard.Query
    ( -- * Query
      Query(..)
    , Answer(..)
      -- * QTerms
    , QTerms(..)
    , parseQTerms
    ) where

import           Data.Char     (isSpace)
import qualified Data.Text     as T

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
      { items      :: [a]
      , offset     :: Word
      , totalCount :: Word
      }
  deriving (Eq, Foldable, Functor, Generic, Ord, Show, Traversable)

-- | Parsed query text.
data QTerms
  = QTerms [Text]
  deriving (Eq, Ord, Show)

parseQTerms :: Text -> Either String QTerms
parseQTerms input = return $ QTerms $ filter (/= "") $ T.split isSpace input
