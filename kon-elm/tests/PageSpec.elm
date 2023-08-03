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
linkHashtags_ = Page.linkHashtagsMarkdown (\t -> "/?q=#" ++ t)

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
                 \_ -> Exp.equal (linkHashtags_ "#hoge") "[#hoge](/?q=#hoge)"
             , test "multiple hashtags" <|
                 \_ -> Exp.equal (linkHashtags_ "#foo bar buzz #quux #hoge") "[#foo](/?q=#foo) bar buzz [#quux](/?q=#quux) [#hoge](/?q=#hoge)"
             , test "hashtag at the start of line" <|
                 \_ -> Exp.equal (linkHashtags_ "quux\n#foobar\n#buzz") "quux\n[#foobar](/?q=#foobar)\n[#buzz](/?q=#buzz)"
             , test "Japanese hashtag" <|
                 \_ -> Exp.equal (linkHashtags_ "  #日本語okハッシュタグ ") "  [#日本語okハッシュタグ](/?q=#日本語okハッシュタグ) "
             , test "hash in hashtag" <|
                 \_ -> Exp.equal (linkHashtags_ " #hoge#fuga") " #hoge#fuga"
             , test "markdown headings" <|
                 \_ -> let input = "# heading 1\n\n## heading 2\n\n### heading 3"
                       in Exp.equal (linkHashtags_ input) input
             , test "fragments in links" <|
                 \_ -> let input = "https://example.com/hoge#foobar\n[external link](http://example.net/#quux)\n[relative fragment link](#hoge)"
                       in Exp.equal (linkHashtags_ input) input
             ]
        ]
