module KonBoard.Util.YAMLSpec (main,spec) where

import Data.Monoid ((<>), mconcat)
import Test.Hspec

import KonBoard.Util.YAML (splitLineBS)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "splitLineBS" $ do
  specify "empty" $ do
    splitLineBS "---" "" `shouldBe` [""]
  specify "line delimiter" $ do
    let input = mconcat $ map (<> "\n")
                [ "foo",
                  "---",
                  "bar",
                  "buzz",
                  "---",
                  "---",
                  "quux",
                  "hoge---",
                  ""
                ]
        expected = [ "foo\n",
                     "bar\nbuzz\n",
                     "",
                     "quux\nhoge---\n\n"
                   ]
    splitLineBS "---" input `shouldBe` expected
  specify "line delimiter at the head" $ do
    let input = mconcat $ map (<> "\n")
                [ "---",
                  "---",
                  "foo"
                ]
        expected = [ "",
                     "",
                     "foo\n"
                   ]
    splitLineBS "---" input `shouldBe` expected
  specify "line delimiter at the end" $ do
    let input = mconcat
                [ "foo\n",
                  "bar\n",
                  "---\n",
                  "buzz\n",
                  "---"
                ]
        expected = [ "foo\nbar\n",
                     "buzz\n",
                     ""
                   ]
    splitLineBS "---" input `shouldBe` expected
