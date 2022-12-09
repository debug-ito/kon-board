-- | (Internal) Utilities related to YAML
--
-- __This module is for internal use. End-users should not use this.__
module KonBoard.Util.Yaml
    ( decodeYAMLDocs
    , readYAMLDocs
    , splitLineBS
    , ArrayOrSingle (..)
    ) where

import           Control.Applicative    ((<$>), (<|>))
import           Control.Exception.Safe (throwIO)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Char8  as BSC
import qualified Data.Foldable          as F
import           Data.Word              (Word8)
import           Data.YAML.Aeson        (decode1Strict)
import           GHC.Generics           (Generic)

-- | (Internal use)
splitLineBS :: ByteString -- ^ delimiter line
             -> ByteString -- ^ data to be split
             -> [ByteString] -- ^ data delimited by the delimiter.
splitLineBS delim_line orig = map BSC.unlines $ splitByLine $ BSC.lines orig
  where
    splitByLine ls = finalize $ foldr f ([], []) ls
      where
        f line (cur_group, ret) = if line == delim_line
                                  then ([], cur_group : ret)
                                  else (line : cur_group, ret)
        finalize (group, ret) = group : ret

isSharpW8 :: Word8 -> Bool
isSharpW8 w = w == 0x23

isSpaceW8 :: Word8 -> Bool
isSpaceW8 w = w == 0x09 || w == 0x0a || w == 0x0d || w == 0x20

isAllSpace :: ByteString -> Bool
isAllSpace s = BS.takeWhile isSpaceW8 s == s

-- | Load data from YAML document, possibly encoded in \"multiple document\" encoding. The 'snd' of
-- the result is the raw YAML document.
decodeYAMLDocs :: FromJSON a => ByteString -> Either String [(a, ByteString)]
decodeYAMLDocs doc = handleError $ traverse decodeWithRaw $ splitYAMLDocs doc
  where
    decodeWithRaw b = (,) <$> decode1Strict b <*> b
    handleError e =
      case e of
        Right r = Right r
        Left err = undefined -- TODO

-- | Split the given into multiple blocks delimited by the line delimiter (@"---"@). Empty blocks
-- are just dropped from the result.
splitYAMLDocs :: ByteString -> [ByteString]
splitYAMLDocs doc = filter (not . isEmptyDoc) $ splitLineBS "---" doc
  where
    isEmptyDoc bs = BS.null $ BS.dropWhile isSpaceW8 bs

readYAMLDocs :: FromJSON a => FilePath -> IO [(a, ByteString)]
readYAMLDocs file = (either throwIO return . decodeYAMLDocs) =<< BS.readFile file


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
