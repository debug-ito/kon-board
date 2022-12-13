module KonBoard.Util.YamlSpec
    ( main
    , spec
    ) where

import           Data.Monoid        (mconcat, (<>))
import           Test.Hspec

import           KonBoard.Util.Yaml (splitYamlDocs)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "splitYamlDocs" $ do
  specify "empty" $ do
    splitYamlDocs "" `shouldBe` []
  specify "non-empty string" $ do
    splitYamlDocs "foo" `shouldBe` ["foo"]
  specify "only comment" $ do
    splitYamlDocs "#bar" `shouldBe` []
  specify "only comment (space prefix)" $ do
    splitYamlDocs "  ## foo" `shouldBe` []
  specify "comment and non-space characters" $ do
    let input = mconcat
                [ "foo\n"
                , "  ## bar\n"
                , "\n"
                , "buzz\n"
                ]
    splitYamlDocs input `shouldBe` [input]
  specify "comment and white spaces" $ do
    let input = mconcat
                [ "\n"
                , "   \n"
                , "## foo"
                , "      \n"
                , "   ## bar buzz"
                , "  \n"
                , "  #quux\n"
                ]
    splitYamlDocs input `shouldBe` []
  specify "line delimiter" $ do
    let input = mconcat $ map (<> "\n")
                [ "foo"
                , "---"
                , "bar"
                , "buzz"
                , "---"
                , "---"
                , "quux"
                , "hoge---"
                , ""
                ]
        expected = [ "foo\n"
                   , "bar\nbuzz\n"
                   , "quux\nhoge---\n\n"
                   ]
    splitYamlDocs input `shouldBe` expected
  specify "line delimiter at the head" $ do
    let input = mconcat $ map (<> "\n")
                [ "---"
                , "---"
                , "foo"
                ]
        expected = ["foo\n"]
    splitYamlDocs input `shouldBe` expected
  specify "line delimiter at the end" $ do
    let input = mconcat
                [ "foo\n"
                , "bar\n"
                , "---\n"
                , "buzz\n"
                , "---"
                ]
        expected = [ "foo\nbar\n"
                   , "buzz\n"
                   ]
    splitYamlDocs input `shouldBe` expected
  specify "preserve comments" $ do
    let input = mconcat
                [ "\n"
                , "# foo bar\n"
                , "foo\n"
                , "  ### foo bar\n"
                , "    foo\n"
                , "---\n"
                , " # foo\n"
                , "bar\n"
                , "## foo\n"
                , "    \"foo ## bar\"\n"
                , "bar\n"
                ]
        expected =
          [ mconcat
            [ "\n"
            , "# foo bar\n"
            , "foo\n"
            , "  ### foo bar\n"
            , "    foo\n"
            ],
            mconcat
            [ " # foo\n"
            , "bar\n"
            , "## foo\n"
            , "    \"foo ## bar\"\n"
            , "bar\n"
            ]
          ]
    splitYamlDocs input `shouldBe` expected
