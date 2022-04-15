module KonBoard.RecipeSpec
    ( main
    , spec
    ) where

import qualified Data.Text       as T
import           Test.Hspec

import           KonBoard.Recipe (Ingredient (..), parseIngredient)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseIngredient" $ do
    let theSpec input expected = do
          specify (T.unpack input) $ do
            (either (const $ Left ()) Right $ parseIngredient input) `shouldBe` expected
    theSpec "foo, bar" (Right $ Ingredient "foo" "bar")
    theSpec "foo  ,  quux  " (Right $ Ingredient "foo" "quux")
    theSpec ",foo" (Left ())
    theSpec "hoge," (Left ())
    theSpec "foo" (Left ())
    theSpec "" (Left ())
