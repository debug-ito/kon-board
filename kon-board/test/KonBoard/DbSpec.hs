module KonBoard.DbSpec
    ( main
    , spec
    ) where

import           Test.Hspec

import           KonBoard.Db               (openSqlite, recipeStoreDb)

import           KonBoard.Recipe.TestStore (makeRecipeStoreSpec)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  specRecipeStore

specRecipeStore :: Spec
specRecipeStore = do
  describe "recipeStoreDb" $ do
    specify "TODO" $ undefined -- TODO
