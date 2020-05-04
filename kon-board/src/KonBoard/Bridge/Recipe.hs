{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}
-- |
-- Module: KonBoard.Bridge.Recipe
-- Description: Easy-to-encode Recipe data types
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module KonBoard.Bridge.Recipe
  ( BRecipeSummary,
    toBRecipeSummary,
    fromBRecipeSummary,
    BRecipeID,
    toBRecipeID,
    fromBRecipeID,
    BRecipe,
    toBRecipe,
    BRecipeIn,
    BRecipeURL,
    BIngDesc,
    toBIngDesc,
    BIngredient,
    toBIngredient
  ) where

import Data.Text (Text)
import qualified Elm.Derive as Elm
import Servant (FromHttpApiData)

import KonBoard.Bridge.Util (dropLabelOptions)
import KonBoard.Recipe
  ( Ingredient(..), IngDesc(..),
    Recipe(..), RecipeBody(..), RecipeIn(..)
  )
import KonBoard.Recipe.Store
  ( RecipeSummary(..), ID
  )

-- | Easy-to-encode version of 'RecipeSummary'.
data BRecipeSummary =
  BRecipeSummary
  { br_id :: BRecipeID,
    br_name :: Text
  }
  deriving (Show,Eq,Ord)

toBRecipeSummary :: RecipeSummary -> BRecipeSummary
toBRecipeSummary (RecipeSummary i n) = BRecipeSummary (toBRecipeID i) n

fromBRecipeSummary :: BRecipeSummary -> RecipeSummary
fromBRecipeSummary (BRecipeSummary i n) = RecipeSummary (fromBRecipeID i) n

newtype BRecipeID = BRecipeID Text
  deriving (Show,Eq,Ord,FromHttpApiData)

toBRecipeID :: ID -> BRecipeID
toBRecipeID = BRecipeID

fromBRecipeID :: BRecipeID -> ID
fromBRecipeID (BRecipeID i) = i

data BRecipe = BRIn BRecipeIn
             | BRURL BRecipeURL
             deriving (Show,Eq,Ord)

toBRecipe :: Recipe -> BRecipe
toBRecipe r =
  case recipeBody r of
    RecipeBodyIn rin ->
      BRIn $ BRecipeIn { bri_name = rname,
                         bri_ings = map toBIngDesc $ recipeIngs rin,
                         bri_desc = recipeDesc rin,
                         bri_ref_url = recipeRefURL rin
                       }
    RecipeBodyURL url ->
      BRURL $ BRecipeURL { bru_name = rname,
                           bru_url = url
                         }
  where
    rname = recipeName r

data BRecipeIn =
  BRecipeIn
  { bri_name :: Text,
    bri_ings :: [BIngDesc],
    bri_desc :: Text,
    bri_ref_url :: Maybe Text
  }
  deriving (Show,Eq,Ord)

data BRecipeURL =
  BRecipeURL
  { bru_name :: Text,
    bru_url :: Text
  }
  deriving (Show,Eq,Ord)


data BIngDesc =
  BIngDesc
  { bid_group :: Maybe Text,
    bid_ings :: [BIngredient]
  }
  deriving (Show,Eq,Ord)


toBIngDesc :: IngDesc -> BIngDesc
toBIngDesc (IngGroup gsym ings) = BIngDesc (Just gsym) $ map toBIngredient ings
toBIngDesc (IngSingle ing) = BIngDesc Nothing [toBIngredient ing]

data BIngredient =
  BIngredient
  { bi_food :: Text,
    bi_qtty :: Text
  }
  deriving (Show,Eq,Ord)


toBIngredient :: Ingredient -> BIngredient
toBIngredient (Ingredient f q) = BIngredient f q


$(Elm.deriveBoth (dropLabelOptions 3) ''BRecipeSummary)
$(Elm.deriveBoth (dropLabelOptions 0) ''BRecipeID)
$(Elm.deriveBoth (dropLabelOptions 0) ''BRecipe)
$(Elm.deriveBoth (dropLabelOptions 4) ''BRecipeIn)
$(Elm.deriveBoth (dropLabelOptions 4) ''BRecipeURL)
$(Elm.deriveBoth (dropLabelOptions 4) ''BIngDesc)
$(Elm.deriveBoth (dropLabelOptions 3) ''BIngredient)
