module MealPhaseSpec exposing (suite)

import Expect as Exp
import Test exposing (Test, describe, test)

import MealPhase exposing (MealPhase(..), toString, fromString)

stringTest : String -> MealPhase -> Test
stringTest str mp =
    test str <|
        let result = Exp.all [parse_result, format_result]
            parse_result _ = Exp.equal (fromString str) (Ok mp)
            format_result _ = Exp.equal (toString mp) str
        in result

suite : Test
suite =
    describe "MealPhase"
    [ describe "toString, fromString"
      [ stringTest "breakfast" Breakfast
      , stringTest "lunch" Lunch
      , stringTest "dinner" Dinner
      , stringTest "@hoge foo bar" (MealOther "hoge foo bar")
      ]
    ]
