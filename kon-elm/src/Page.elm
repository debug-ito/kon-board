module Page exposing
    ( Page(..)
    , parseUrl
    , recipePageLink
    )

import Url exposing (Url)
import Url.Parser exposing (Parser, oneOf, (</>))
import Url.Parser as P
import Url.Builder as B

import Bridge exposing (BRecipeID)

{- | The page associated with URL.
-}
type Page =
      -- | The top page
      PageTop
      -- | The recipe page
    | PageRecipe BRecipeID

parseUrl : Url -> Maybe Page
parseUrl = P.parse parserPage

parserPage : Parser (Page -> a) a
parserPage =
    oneOf
    [ P.map PageTop P.top
    , P.map PageRecipe (P.s "recipes" </> P.string)
    ]

recipePageLink : BRecipeID -> String
recipePageLink rid = B.absolute ["recipes", rid] []
