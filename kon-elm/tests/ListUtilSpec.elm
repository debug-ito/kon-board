module ListUtilSpec exposing
    ( suite
    )

import Expect as Exp
import Test exposing (Test, describe, test)
import Tuple exposing (first)

import ListUtil exposing (replaceOrAdd, blocks)

eq1 : (a, b) -> (a, b) -> Bool
eq1 l r = first l == first r

repOA : (Int, String) -> List (Int, String) -> List (Int, String)
repOA input l = replaceOrAdd (eq1 input) input l

suite : Test
suite =
    describe "ListUtil"
        [ suite_replaceOrAdd
        , suite_blocks
        ]
        
suite_replaceOrAdd : Test
suite_replaceOrAdd =
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

suite_blocks : Test
suite_blocks =
    describe "blocks"
        [ test "empty" <|
              \_ -> Exp.equal (blocks 3 []) []
        , test "one" <|
            \_ -> Exp.equal (blocks 3 [1]) [[1]]
        , test "two" <|
            \_ -> Exp.equal (blocks 3 [1,2]) [[1,2]]
        , test "three" <|
            \_ -> Exp.equal (blocks 3 [1,2,3]) [[1,2,3]]
        , test "four" <|
            \_ -> Exp.equal (blocks 3 [1,2,3,4]) [[1], [2,3,4]]
        , test "three groups" <|
            \_ -> Exp.equal (blocks 3 [1,2,3,4,5,6,7,8]) [[1,2], [3,4,5], [6,7,8]]
        , test "1-elem groups" <|
            \_ -> Exp.equal (blocks 1 [1,2,3,4]) [[1], [2], [3], [4]]
        ]
