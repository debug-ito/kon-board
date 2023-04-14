{-# LANGUAGE TemplateHaskell #-}
-- | Easy-to-encode Recipe data types
module KonBoard.Bridge.Recipe
    ( BRecipeStored
    , toBRecipeStored
    , BRecipeId
    , toBRecipeId
    , fromBRecipeId
    , BRef
    , toBRef
    , BIngDesc
    , toBIngDesc
    , BIngredient
    , toBIngredient
    , BIngGrouped
    , BAnswerRecipe
    , toBAnswerRecipe
    ) where

import           Elm.Derive          (deriveBoth)
import           Servant.API         (FromHttpApiData)

import           KonBoard.Base       (HasField (..), Text)
import           KonBoard.Query      (Answer (..))
import           KonBoard.Recipe     (Id, IngDesc (..), Ingredient (..), Recipe, RecipeStored (..),
                                      Ref (..))
import           KonBoard.Util.Aeson (dropLabelOptions)

data BIngredient
  = BIngredient
      { food :: Text
      , qtty :: Text
      }
  deriving (Eq, Ord, Show)

toBIngredient :: Ingredient -> BIngredient
toBIngredient (Ingredient f q) = BIngredient { food = f, qtty = q }

data BIngGrouped
  = BIngGrouped
      { g    :: Text
      , ings :: [BIngredient]
      }
  deriving (Eq, Ord, Show)

data BIngDesc
  = BIngGroup BIngGrouped
  | BIngSingle BIngredient
  deriving (Eq, Ord, Show)

toBIngDesc :: IngDesc -> BIngDesc
toBIngDesc d =
  case d of
    IngGroup gr is -> BIngGroup (BIngGrouped { g = gr, ings = map toBIngredient $ is })
    IngSingle i    -> BIngSingle $ toBIngredient i

data BRef
  = BRef
      { source :: Maybe Text
      , url    :: Maybe Text
      }
  deriving (Eq, Ord, Show)

toBRef :: Ref -> BRef
toBRef r =
  case r of
    RefSource s -> BRef { source = Just s, url = Nothing }
    RefUrl u ms -> BRef { source = ms, url = Just u }

newtype BRecipeId
  = BRecipeId Text
  deriving (Eq, FromHttpApiData, Ord, Show)

toBRecipeId :: Id -> BRecipeId
toBRecipeId = BRecipeId

fromBRecipeId :: BRecipeId -> Id
fromBRecipeId (BRecipeId i) = i

data BRecipeStored
  = BRecipeStored
      { id   :: BRecipeId
      , name :: Text
      , ings :: [BIngDesc]
      , desc :: Text
      , ref  :: [BRef]
      }
  deriving (Eq, Ord, Show)

toBRecipeStored :: RecipeStored -> BRecipeStored
toBRecipeStored rs =
  BRecipeStored
  { id = toBRecipeId $ getField @"id" rs
  , name = getField @"name" r
  , ings = map toBIngDesc $ getField @"ingredients" r
  , desc = getField @"description" r
  , ref = map toBRef $ getField @"references" r
  }
  where
    r = getField @"recipe" rs

data BAnswerRecipe
  = BAnswerRecipe
      { recipes  :: [BRecipeStored]
      , has_next :: Bool
      }
  deriving (Eq, Ord, Show)

toBAnswerRecipe :: Answer RecipeStored -> BAnswerRecipe
toBAnswerRecipe a = BAnswerRecipe { recipes = map toBRecipeStored $ getField @"items" a
                                  , has_next = getField @"hasNext" a
                                  }

$(deriveBoth (dropLabelOptions 0) ''BRecipeStored)
$(deriveBoth (dropLabelOptions 0) ''BRecipeId)
$(deriveBoth (dropLabelOptions 0) ''BRef)
$(deriveBoth (dropLabelOptions 0) ''BIngGrouped)
$(deriveBoth (dropLabelOptions 0) ''BIngDesc)
$(deriveBoth (dropLabelOptions 0) ''BIngredient)
$(deriveBoth (dropLabelOptions 0) ''BAnswerRecipe)
