module Page exposing
    ( Page(..)
    , CalendarView(..)
    , initPage
    , parseUrl
    , recipePageLink
    )

import Url exposing (Url)
import Url.Parser exposing (Parser, oneOf, (</>))
import Url.Parser as P
import Url.Builder as B

import Bridge exposing (BRecipeID)
import Coming exposing (Coming(..))

{- | The page associated with URL.
-}
type Page =
      -- | The top page
      PageTop ViewportAdjusted CalendarView
      -- | The recipe page
    | PageRecipe BRecipeID

type alias ViewportAdjusted = Coming String ()

type CalendarView = CalViewList
                  | CalViewTable

initPage : Page
initPage = PageTop NotStarted CalViewList

parseUrl : Url -> Maybe Page
parseUrl = P.parse parserPage

parserPage : Parser (Page -> a) a
parserPage =
    oneOf
    [ P.map (PageTop NotStarted CalViewList) P.top
    , P.map PageRecipe (P.s "recipes" </> P.string)
    ]

recipePageLink : BRecipeID -> String
recipePageLink rid = B.absolute ["recipes", rid] []
