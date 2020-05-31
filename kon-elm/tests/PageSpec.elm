module PageSpec exposing (suite)

import Expect as Exp
import Maybe
import Test exposing (Test, describe, test)
import Url exposing (Url)
import Url

import Coming exposing (Coming(..))
import Page exposing (Page(..))
import Page

parseUrl_ : String -> Maybe Page
parseUrl_ s = Url.fromString s |> Maybe.andThen
              ( \url -> Page.parseUrl url
              )

suite : Test
suite =
    describe "Page"
        [ describe "parseUrl"
              [ test "top page" <|
                    \_ -> Exp.equal (parseUrl_ "http://example.com/") (Just <| PageTop NotStarted)
              , test "recipe page" <|
                  \_ -> Exp.equal (parseUrl_ "http://192.168.0.1/recipes/foobar") (Just <| PageRecipe "foobar")
              ]
        ]
