module Recipe exposing
    ( recipeName
    )

{- | Utilities about Recipes -}

import Bridge exposing (BRecipe(..))

recipeName : BRecipe -> String
recipeName br =
    case br of
        BRIn rin -> rin.name
        BRURL ru -> ru.name
