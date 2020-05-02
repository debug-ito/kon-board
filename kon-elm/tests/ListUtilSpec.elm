module ListUtilSpec exposing
    ( suite
    )

import Expect as Exp
import Test exposing (Test, describe)
import Tuple exposing (first)

import ListUtil exposing (replaceOrAdd)

eq1 : (a, b) -> (a, b) -> Bool
eq1 l r = first l == first r

repOA : (Int, String) -> List (Int, String) -> List (Int, String)
repOA input l = replaceOrAdd (eq1 input) input l
          
suite : Test
suite =
    describe "replaceOrAdd"
        [ test "add to empty" <|
            \_ -> Exp.equal (repOA (10, "foo") []) ([(10, "foo")])
        , test "no match" <|
            \_ ->
                let input = (1, "a")
                    orig = [(2, "b"), (3, "c"), (4, "d")]
                    expected = [(1, "a"), (2, "b"), (3, "c"), (4, "d")]
                in Exp.equal (repOA input orig) expected
        , test "matches" <|
            \_ ->
                let input = (1, "a")
                    orig = [(2, "b"), (1, "c"), (1, "d"), (3, "e"), (4, "f")]
                    expected = [(2, "b"), (1, "a"), (1, "a"), (3, "e"), (4, "f")]
                in Exp.equal (repOA input orig) expected
        ]
