module KonBoard.Util.Aeson
    ( dropLabelOptions
    ) where

import           Data.Aeson (Options (fieldLabelModifier, omitNothingFields, sumEncoding),
                             SumEncoding (..), defaultOptions)

dropLabelOptions :: Int -> Options
dropLabelOptions drop_num =
  defaultOptions
  { fieldLabelModifier = drop drop_num
  , sumEncoding = UntaggedValue
  , omitNothingFields = True
  }
