module PageSpec exposing (suite)

import Date exposing (fromCalendarDate)
import Expect as Exp
import Maybe
import Test exposing (Test, describe, test)
import Time exposing (Month(..))
import Url exposing (Url)
import Url

import Coming exposing (Coming(..))
import Page exposing (Page(..))
import Page

parseUrl_ : String -> Maybe Page
parseUrl_ s = Url.fromString s |> Maybe.andThen
              ( \url -> Page.parseUrl url
              )

linkHashtags_ : String -> String
linkHashtags_ = Page.linkHashtagsMarkdown (\t -> "/q=#" ++ t)

suite : Test
suite =
    describe "Page"
        [ describe "parseUrl"
              [ test "top page" <|
                    \_ ->
                        let pt = { viewportAdjusted = NotStarted, currentAnchor = NotStarted }
                        in Exp.equal (parseUrl_ "http://example.com/") (Just <| PageTop pt)
              , test "recipe page" <|
                  \_ -> let got = parseUrl_ "http://192.168.0.1/recipes/foobar"
                            expected = Just <| PageRecipe { recipeID = "foobar", recipe = NotStarted }
                        in Exp.equal got expected
              , test "day page" <|
                  \_ -> let got = parseUrl_ "http://192.168.1.1/days/2020-07-12"
                            expected = Just <| PageDay { day = fromCalendarDate 2020 Jul 12, calEntry = NotStarted }
                        in Exp.equal got expected
              ]
        , describe "linkHashtagsMarkdown"
             [ test "empty" <|
                 \_ -> Exp.equal (linkHashtags_ "") ""
             , test "a hashtag" <|
                 \_ -> Exp.equal (linkHashtags_ "#hoge") "/q=#hoge"
             ]
        ]
