module KonBoard.Util.YamlSpec
    ( main
    , spec
    ) where

import           Data.Monoid        (mconcat, (<>))
import           Test.Hspec

import           KonBoard.Util.Yaml (splitLineBS)

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
  specify "preserve comments" $ do
    let input = mconcat
                [ "\n",
                  "# foo bar\n",
                  "foo\n",
                  "  ### foo bar\n",
                  "    foo\n",
                  "---\n",
                  " # foo\n",
                  "bar\n",
                  "## foo\n",
                  "    \"foo ## bar\"\n",
                  "bar\n"
                ]
        expected =
          [ mconcat
            [ "\n",
              "# foo bar\n",
              "foo\n",
              "  ### foo bar\n",
              "    foo\n"
            ],
            mconcat
            [ " # foo\n",
              "bar\n",
              "## foo\n",
              "    \"foo ## bar\"\n",
              "bar\n"
            ]
          ]
    splitLineBS "---" input `shouldBe` expected
