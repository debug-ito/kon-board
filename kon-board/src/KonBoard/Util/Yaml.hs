-- | (Internal) Utilities related to YAML
--
-- __This module is for internal use. End-users should not use this.__
module KonBoard.Util.Yaml
    ( decodeYaml
    , splitYamlDocs
    , ArrayOrSingle (..)
    ) where

import           Control.Applicative    ((<$>), (<|>))
import           Control.Exception.Safe (throwString)
import           Data.Aeson             (FromJSON (..), ToJSON (..))
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Char8  as BSC
import qualified Data.ByteString.Lazy   as BSL
import qualified Data.Foldable          as F
import           Data.Word              (Word8)
import           Data.YAML              (prettyPosWithSource)
import           Data.YAML.Aeson        (decode1)
import           GHC.Generics           (Generic)

decodeYaml :: FromJSON a => ByteString -> Either String a
decodeYaml b = handleError $ decode1 lb
  where
    lb = BSL.fromStrict b
    handleError e =
      case e of
        Right r         -> Right r
        Left (pos, err) -> Left $ prettyPosWithSource pos lb ("error: " ++ err)

-- | Split the given document data into multiple blocks delimited by the line delimiter
-- (@"---"@). Empty blocks are just dropped from the result. A block that only contains YAML
-- comments is considered "empty", so it's removed from the result.
splitYamlDocs :: ByteString -> [ByteString]
splitYamlDocs doc = map BSC.unlines $ filter (not . isEmptyDoc) $ groupBySep (== "---") $ BSC.lines doc
  where
    isEmptyDoc block = BS.null $ BS.dropWhile isSpaceW8 $ BSC.unlines $ map removeComment $ block
    removeComment :: ByteString -> ByteString
    removeComment line = undefined -- TODO: remove comment from the line.

-- | Given a stream of @a@, separate the @a@s into blocks, delimited by a specific @a@ as the
-- delimiter. The delimiter is excluded from the result.
groupBySep :: (Foldable t)
           => (a -> Bool) -- ^ Returns 'True' if the input is the block delimiter
           -> t a
           -> [[a]]
groupBySep isDelim input = finalize $ foldr f ([], []) input
  where
    f e (curGroup, ret) = if isDelim e
                          then ([]          , curGroup : ret)
                          else (e : curGroup, ret)
    finalize (group, ret) = group : ret

isSpaceW8 :: Word8 -> Bool
isSpaceW8 w = w == 0x09 || w == 0x0a || w == 0x0d || w == 0x20

-- | (internal use) A JSON/YAML encoding wrapper that is either a
-- single element or an array.
data ArrayOrSingle a
  = AOSSingle a
  | AOSArray [a]
  deriving (Eq, Generic, Ord, Show)

instance F.Foldable ArrayOrSingle where
  foldr f acc (AOSSingle a) = f a acc
  foldr f acc (AOSArray as) = foldr f acc as

instance FromJSON a => FromJSON (ArrayOrSingle a) where
  parseJSON v = (AOSArray <$> parseJSON v)
                <|> (AOSSingle <$> parseJSON v)

instance ToJSON a => ToJSON (ArrayOrSingle a) where
  toJSON (AOSSingle a) = toJSON a
  toJSON (AOSArray as) = toJSON as
