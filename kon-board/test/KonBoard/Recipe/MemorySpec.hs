module KonBoard.Recipe.MemorySpec
    ( main
    , spec
    ) where

import           Control.Monad.Logger   (LoggingT)
import           Control.Monad.Trans    (MonadIO (..))
import           Data.Foldable          (traverse_)
import           Data.List              (nub)
import           Data.Maybe             (fromJust)
import qualified Data.Text              as T
import           GHC.Records            (HasField (..))
import           Test.Hspec

import           KonBoard.Recipe        (Id, Name, Recipe (..), RecipeStore (..), RecipeStored (..))
import           KonBoard.Recipe.Memory (recipeStoreMemory)
import           KonBoard.Recipe.Yaml   (loadYamlFile)

import           KonBoard.TestLogger    (basicLogging)

main :: IO ()
main = hspec spec

spec :: Spec
spec = specStore

openStore :: [FilePath] -> LoggingT IO (RecipeStore (LoggingT IO))
openStore files = do
  rs <- recipeStoreMemory
  traverse_ (loadYamlFile rs) $ map ("test/recipes/" <>) files
  return rs

idForStore :: Name -> [FilePath] -> LoggingT IO Id
idForStore n files = do
  store <- openStore $ files
  fmap (getField @"id" . fromJust) $ getRecipeByName store n

loadAndCheckName :: (MonadIO m, MonadFail m) => RecipeStore m -> Name -> m ()
loadAndCheckName store inputName = do
  (Just rs) <- getRecipeByName store inputName
  liftIO $ (getField @"name" $ getField @"recipe" rs) `shouldBe` inputName
  let rid = getField @"id" rs
  (Just rsById) <- getRecipeById store rid
  liftIO $ rsById `shouldBe` rs
  liftIO $ putStrLn ("Recipe: '" <> T.unpack inputName <> "' -> ID: " <> T.unpack rid)

specStore :: Spec
specStore = describe "RecipeStore" $ do
  let commonYamlFiles = [ "recipe_in.yaml"
                        , "recipe_in_url.yaml"
                        , "recipe_multi.yaml"
                        , "recipe_url.yaml"
                        ]
  specify "getRecipeByName, getRecipeById" $ basicLogging $ do
    store <- openStore commonYamlFiles
    loadAndCheckName store "internal recipe with ingredient groups"
    loadAndCheckName store "external recipe with URL"
    loadAndCheckName store "recipe 2"
  specify "loadRecipeByName, not found" $ basicLogging $ do
    store <- openStore commonYamlFiles
    got <- getRecipeByName store "this should not exist"
    liftIO $ got `shouldBe` Nothing
  specify "recipeID has to be stable regardless of store" $ basicLogging $ do
    let input = [ ["recipe_multi.yaml"],
                  ["recipe_in.yaml", "recipe_url.yaml", "recipe_in_url.yaml", "recipe_multi.yaml"],
                  ["recipe_url.yaml", "recipe_in.yaml", "recipe_multi.yaml", "recipe_in_url.yaml"]
                ]
    got <- traverse (idForStore "recipe 2") $ input
    liftIO $ (length $ nub got) `shouldBe` 1