module DateUtilSpec exposing (suite)

import Expect as Exp
import Test exposing (Test, describe, test)

import Date exposing (Date, fromCalendarDate)
import Time exposing (Weekday(..), Month(..))

import DateUtil exposing (nearbyWeekday)

date : Int -> Month -> Int -> Date
date = fromCalendarDate

suite : Test
suite =
    describe "nearbyWeekday"
        [ test "if n == 0, it should return the start date" <|
              \_ ->
              Exp.equal (nearbyWeekday (date 2020 May 31) Thu 0) (date 2020 May 31)
        , test "n == 1, lower Weekday" <|
            \_ ->
                Exp.equal (nearbyWeekday (date 2020 May 27) Sat 1) (date 2020 May 30)
        , test "n == 1, equal Weekday" <|
            \_ ->
                Exp.equal (nearbyWeekday (date 2020 May 30) Sat 1) (date 2020 Jun 6)
        , test "n == 1, higher Weekday" <|
            \_ ->
                Exp.equal (nearbyWeekday (date 2020 May 31) Sat 1) (date 2020 Jun 6)
        , test "n == 2, lower Weekday" <|
            \_ ->
                Exp.equal (nearbyWeekday (date 2020 May 27) Sat 2) (date 2020 Jun 6)
        , test "n == 2, equal Weekday" <|
            \_ ->
                Exp.equal (nearbyWeekday (date 2020 May 30) Sat 2) (date 2020 Jun 13)
        , test "n == 2, higher Weekday" <|
            \_ ->
                Exp.equal (nearbyWeekday (date 2020 May 31) Sat 2) (date 2020 Jun 13)
        , test "n == -1, higher Weekday" <|
            \_ ->
                Exp.equal (nearbyWeekday (date 2020 May 30) Thu -1) (date 2020 May 28)
        , test "n == -1, equal Weekday" <|
            \_ ->
                Exp.equal (nearbyWeekday (date 2020 May 28) Thu -1) (date 2020 May 21)
        , test "n == -1, lower Weekday" <|
            \_ ->
                Exp.equal (nearbyWeekday (date 2020 May 26) Thu -1) (date 2020 May 21)
        , test "n == -2, higher Weekday" <|
            \_ ->
                Exp.equal (nearbyWeekday (date 2020 May 30) Thu -2) (date 2020 May 21)
        , test "n == -2, equal Weekday" <|
            \_ ->
                Exp.equal (nearbyWeekday (date 2020 May 28) Thu -2) (date 2020 May 14)
        , test "n == -2, lower Weekday" <|
            \_ ->
                Exp.equal (nearbyWeekday (date 2020 May 26) Thu -2) (date 2020 May 14)
        ]
