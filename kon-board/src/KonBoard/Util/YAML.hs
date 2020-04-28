{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: KonBoard.Util.YAML
-- Description: (Internal) Utilities related to YAML
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __This module is for internal use. End-users should not use this.__
module KonBoard.Util.YAML
  ( decodeYAMLDocs,
    readYAMLDocs,
    splitLineBS
  ) where

import Control.Exception.Safe (throwIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Yaml (FromJSON, ParseException, decodeEither')

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

-- | Load data from YAML document, possibly encoded in \"multiple
-- document\" encoding.
decodeYAMLDocs :: FromJSON a => ByteString -> Either ParseException [a]
decodeYAMLDocs doc = traverse decodeEither' $ filter (not . isEmptyDoc) $ splitLineBS "---" doc
  where
    isEmptyDoc bs = BS.null $ BS.dropWhile isSpaceW8 bs
    isSpaceW8 w = w == 0x09 || w == 0x0a || w == 0x0d || w == 0x20

readYAMLDocs :: FromJSON a => FilePath -> IO [a]
readYAMLDocs file = (either throwIO return . decodeYAMLDocs) =<< BS.readFile file
