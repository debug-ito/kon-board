module KonBoard.QuerySpec
    ( main
    , spec
    ) where

import           KonBoard.Query (QTerms (..), parseQTerms)

import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseQTerms" $ do
    specify "empty" $ do
      parseQTerms "" `shouldBe` Right (QTerms [])
    specify "only spaces" $ do
      parseQTerms "   " `shouldBe` Right (QTerms [])
    specify "one word" $ do
      parseQTerms "hoge" `shouldBe` Right (QTerms ["hoge"])
    specify "one word wrapped by spaces" $ do
      parseQTerms "  hoge  " `shouldBe` Right (QTerms ["hoge"])
    specify "two words" $ do
      parseQTerms " foo bar" `shouldBe` Right (QTerms ["foo", "bar"])
