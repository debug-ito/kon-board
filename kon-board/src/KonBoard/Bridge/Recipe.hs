{-# LANGUAGE TemplateHaskell #-}
-- | Easy-to-encode Recipe data types
module KonBoard.Bridge.Recipe
    ( BRecipeStored
    , toRecipeStored
    , BRecipeId
    , BRef
    , toBRef
    , BIngDesc
    , toBIngDesc
    , BIngredient
    , toBIngredient
    ) where

import           KonBoard.Base   (HasField (..))
import           KonBoard.Recipe (Ingredient (..))


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

toBIngDesc :: IngDesc -> BIngGroup
toBIngDesc d =
  case d of
    IngGroup gr is = BIngGroup (BIngGrouped { g = gr, ings = is })
    IngSingle i = BIngSingle $ toBIngredient i

data BRef
  = BRef
      { source :: Maybe Text
      , url    :: Maybe Text
      }
  deriving (Eq, Ord, Show)

toBRef :: Ref -> BRef
toBRef r =
  case r of
    RefSource s = BRef { source = Just s, url = Nothing }
    RefUrl u ms = BRef { source = ms, url = Just u }

type BRecipeId = Text

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
  { id = getField @"id" rs
  , name = getField @"name" r
  , ings = getField @"ingredients" r
  , desc = getField @"description" r
  , ref = getField @"references" r
  }
  where
    r = getField @"recipe" rs

$(Elm.deriveBoth (dropLabelOptions 0) ''BRecipeStored)
$(Elm.deriveBoth (dropLabelOptions 0) ''BRecipeId)
$(Elm.deriveBoth (dropLabelOptions 0) ''BRef)
$(Elm.deriveBoth (dropLabelOptions 0) ''BIngDesc)
$(Elm.deriveBoth (dropLabelOptions 0) ''BIngredient)

---- import           Data.Text             (Text)
---- import qualified Elm.Derive            as Elm
---- import           Servant.API           (FromHttpApiData)
----
---- import           KonBoard.Bridge.Util  (dropLabelOptions)
---- import           KonBoard.Recipe       (IngDesc (..), Ingredient (..), Recipe (..), RecipeBody (..),
----                                         RecipeExt (..), RecipeIn (..))
---- import           KonBoard.Recipe.Store (ID, RecipeSummary (..))
----
---- -- | Easy-to-encode version of 'RecipeSummary'.
---- data BRecipeSummary
----   = BRecipeSummary
----       { br_id   :: BRecipeID
----       , br_name :: Text
----       }
----   deriving (Eq, Ord, Show)
----
---- toBRecipeSummary :: RecipeSummary -> BRecipeSummary
---- toBRecipeSummary (RecipeSummary i n) = BRecipeSummary (toBRecipeID i) n
----
---- fromBRecipeSummary :: BRecipeSummary -> RecipeSummary
---- fromBRecipeSummary (BRecipeSummary i n) = RecipeSummary (fromBRecipeID i) n
----
---- newtype BRecipeID
----   = BRecipeID Text
----   deriving (Eq, FromHttpApiData, Ord, Show)
----
---- toBRecipeID :: ID -> BRecipeID
---- toBRecipeID = BRecipeID
----
---- fromBRecipeID :: BRecipeID -> ID
---- fromBRecipeID (BRecipeID i) = i
----
---- data BRecipe
----   = BRIn BRecipeIn
----   | BRExt BRecipeExt
----   | BRURL BRecipeURL
----   deriving (Eq, Ord, Show)
----
---- toBRecipe :: Recipe -> BRecipe
---- toBRecipe r =
----   case recipeBody r of
----     RecipeBodyIn rin ->
----       BRIn $ BRecipeIn { bri_name = rname,
----                          bri_ings = map toBIngDesc $ recipeIngs rin,
----                          bri_desc = recipeDesc rin,
----                          bri_ref_url = recipeRefURL rin
----                        }
----     RecipeBodyURL url ->
----       BRURL $ BRecipeURL { bru_name = rname,
----                            bru_url = url
----                          }
----     RecipeBodyExt re ->
----       BRExt $ BRecipeExt { bre_name = rname,
----                            bre_source = recipeSource re,
----                            bre_ext_url = recipeExtURL re
----                          }
----   where
----     rname = recipeName r
----
---- data BRecipeIn
----   = BRecipeIn
----       { bri_name    :: Text
----       , bri_ings    :: [BIngDesc]
----       , bri_desc    :: Text
----       , bri_ref_url :: Maybe Text
----       }
----   deriving (Eq, Ord, Show)
----
---- data BRecipeExt
----   = BRecipeExt
----       { bre_name    :: Text
----       , bre_source  :: Text
----       , bre_ext_url :: Maybe Text
----       }
----   deriving (Eq, Ord, Show)
----
---- data BRecipeURL
----   = BRecipeURL
----       { bru_name :: Text
----       , bru_url  :: Text
----       }
----   deriving (Eq, Ord, Show)
----
----
---- data BIngDesc
----   = BIngDesc
----       { bid_group :: Maybe Text
----       , bid_ings  :: [BIngredient]
----       }
----   deriving (Eq, Ord, Show)
----
----
---- toBIngDesc :: IngDesc -> BIngDesc
---- toBIngDesc (IngGroup gsym ings) = BIngDesc (Just gsym) $ map toBIngredient ings
---- toBIngDesc (IngSingle ing)      = BIngDesc Nothing [toBIngredient ing]
----
---- data BIngredient
----   = BIngredient
----       { bi_food :: Text
----       , bi_qtty :: Text
----       }
----   deriving (Eq, Ord, Show)
----
----
---- toBIngredient :: Ingredient -> BIngredient
---- toBIngredient (Ingredient f q) = BIngredient f q
----
----
---- $(Elm.deriveBoth (dropLabelOptions 3) ''BRecipeSummary)
---- $(Elm.deriveBoth (dropLabelOptions 0) ''BRecipeID)
---- $(Elm.deriveBoth (dropLabelOptions 0) ''BRecipe)
---- $(Elm.deriveBoth (dropLabelOptions 4) ''BRecipeIn)
---- $(Elm.deriveBoth (dropLabelOptions 4) ''BRecipeExt)
---- $(Elm.deriveBoth (dropLabelOptions 4) ''BRecipeURL)
---- $(Elm.deriveBoth (dropLabelOptions 4) ''BIngDesc)
---- $(Elm.deriveBoth (dropLabelOptions 3) ''BIngredient)
----
