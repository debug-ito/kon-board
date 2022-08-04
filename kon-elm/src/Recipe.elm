module Recipe exposing
    ( recipeName
    )

{- | Utilities about Recipes -}

import Bridge exposing (BRecipeStored)

recipeName : BRecipeStored -> String
recipeName r = r.name
