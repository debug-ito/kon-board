module KonBoard.Bridge.TimeSpec
    ( main
    , spec
    ) where

import           Data.Time   (fromGregorian)
import           Servant.API (ToHttpApiData (..))
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "BDay" $ do
    specify "toBDay" $ do
      (toQueryParam $ toBDay $ fromGregorian 2019 5 11) `shouldBe` "2019-05-11"
    specify "fromBDay" $ do
      (fromBDay $ either (const $ error "this should not happen") id $ parseQueryParam "2019-11-03") `shouldBe` (Right $ fromGregorian 2019 11 3)
