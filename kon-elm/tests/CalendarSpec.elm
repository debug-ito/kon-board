module CalendarSpec exposing
    ( suite
    )

{-| Unit tests -}

import Expect as Exp
import Date exposing (fromCalendarDate, Date)
import String
import Test exposing (Test, describe, test)
import Time
import Time exposing (Weekday(..), Month(..))

import Bridge exposing (BMealPlan)
import Calendar exposing (Calendar)
import Calendar as Cal
import MealPhase exposing (MealPhase(..))

calDate : Int -> Month -> Int -> Date
calDate = fromCalendarDate

emptyCE : Int -> Month -> Int -> Cal.CalEntry
emptyCE y m d = { day = calDate y m d, meals = [] }

periodT : (Calendar, a, b) -> ((Date, Date), a, b)
periodT (cal, a, b) = (Cal.startAndEnd cal, a, b)

suite : Test
suite =
    describe "Calendar"
        [ describe "forWeeks"
              [ test "not on start_wday" <|
                    \_ ->
                    let got = Cal.forWeeks (calDate 2020 Jul 1) Sun 1 1
                        exp_period = (calDate 2020 Jun 28, calDate 2020 Jul 5)
                        exp_es = [ emptyCE 2020 Jun 28
                                 , emptyCE 2020 Jun 29
                                 , emptyCE 2020 Jun 30
                                 , emptyCE 2020 Jul 1
                                 , emptyCE 2020 Jul 2
                                 , emptyCE 2020 Jul 3
                                 , emptyCE 2020 Jul 4
                                 ]
                    in Exp.all [ (\_ -> Exp.equal (Cal.startAndEnd got) exp_period)
                               , (\_ -> Exp.equal (Cal.entries got) exp_es)
                               ] got
              , test "on start_wday" <|
                  \_ ->
                      let got = Cal.forWeeks (calDate 2020 Jul 9) Thu 1 1
                          exp_period = (calDate 2020 Jul 2, calDate 2020 Jul 16)
                          exp_es = [ emptyCE 2020 Jul 2
                                   , emptyCE 2020 Jul 3
                                   , emptyCE 2020 Jul 4
                                   , emptyCE 2020 Jul 5
                                   , emptyCE 2020 Jul 6
                                   , emptyCE 2020 Jul 7
                                   , emptyCE 2020 Jul 8
                                   , emptyCE 2020 Jul 9
                                   , emptyCE 2020 Jul 10
                                   , emptyCE 2020 Jul 11
                                   , emptyCE 2020 Jul 12
                                   , emptyCE 2020 Jul 13
                                   , emptyCE 2020 Jul 14
                                   , emptyCE 2020 Jul 15
                                   ]
                      in Exp.all [ (\_ -> Exp.equal (Cal.startAndEnd got) exp_period)
                                 , (\_ -> Exp.equal (Cal.entries got) exp_es)
                                 ] got
              , test "multiple weeks" <|
                  \_ ->
                      let got = Cal.forWeeks (calDate 2020 Jul 1) Sat 2 2
                          exp_period = (calDate 2020 Jun 20, calDate 2020 Jul 11)
                          exp_es = [ emptyCE 2020 Jun 20
                                   , emptyCE 2020 Jun 21
                                   , emptyCE 2020 Jun 22
                                   , emptyCE 2020 Jun 23
                                   , emptyCE 2020 Jun 24
                                   , emptyCE 2020 Jun 25
                                   , emptyCE 2020 Jun 26
                                   , emptyCE 2020 Jun 27
                                   , emptyCE 2020 Jun 28
                                   , emptyCE 2020 Jun 29
                                   , emptyCE 2020 Jun 30
                                   , emptyCE 2020 Jul 1
                                   , emptyCE 2020 Jul 2
                                   , emptyCE 2020 Jul 3
                                   , emptyCE 2020 Jul 4
                                   , emptyCE 2020 Jul 5
                                   , emptyCE 2020 Jul 6
                                   , emptyCE 2020 Jul 7
                                   , emptyCE 2020 Jul 8
                                   , emptyCE 2020 Jul 9
                                   , emptyCE 2020 Jul 10
                                   ]
                      in Exp.all [ (\_ -> Exp.equal (Cal.startAndEnd got) exp_period)
                                 , (\_ -> Exp.equal (Cal.entries got) exp_es)
                                 ] got
              ]
        , describe "addMealPlan"
              [ test "no match" <|
                    \_ ->
                    let input = { year = 2019, month = 4, day = 20
                                , phase = "lunch"
                                , recipes = [{id = "foo", name = "foo name", ings = [], desc = "", ref = []}]
                                , notes = []
                                }
                        start_cal = Cal.forWeeks (fromCalendarDate 2019 Time.May 20) Time.Sun 2 2
                        got = Cal.addMealPlan input start_cal
                    in case got of
                           Ok _ -> Exp.fail "the result should fail."
                           Err e -> Exp.onFail ("Unexpected error message: " ++ e) <| Exp.equal True (String.contains "Cannot find CalEntry for 2019-04-20" e)
              , test "add single match" <|
                  \_ ->
                      let bm = { year = 2019, month = 4, day = 20
                               , phase = "lunch"
                               , recipes = [addedRecipe]
                               , notes = ["a note"]
                               }
                          addedRecipe = { id = "foo"
                                        , name = "foo name"
                                        , ings = []
                                        , desc = "foo desc"
                                        , ref = []
                                        }
                          start_cal =
                              Cal.forWeeks (fromCalendarDate 2019 Time.Apr 17) Time.Sun 2 1
                          expected = [ { day = fromCalendarDate 2019 Time.Apr 14
                                       , meals = []
                                       }
                                     , { day = fromCalendarDate 2019 Time.Apr 15
                                       , meals = []
                                       }
                                     , { day = fromCalendarDate 2019 Time.Apr 16
                                       , meals = []
                                       }
                                     , { day = fromCalendarDate 2019 Time.Apr 17
                                       , meals = []
                                       }
                                     , { day = fromCalendarDate 2019 Time.Apr 18
                                       , meals = []
                                       }
                                     , { day = fromCalendarDate 2019 Time.Apr 19
                                       , meals = []
                                       }
                                     , { day = fromCalendarDate 2019 Time.Apr 20
                                       , meals = [{phase = Lunch, recipes = [addedRecipe], notes = ["a note"]}]
                                       }
                                     , { day = fromCalendarDate 2019 Time.Apr 21
                                       , meals = []
                                       }
                                     , { day = fromCalendarDate 2019 Time.Apr 22
                                       , meals = []
                                       }
                                     , { day = fromCalendarDate 2019 Time.Apr 23
                                       , meals = []
                                       }
                                     , { day = fromCalendarDate 2019 Time.Apr 24
                                       , meals = []
                                       }
                                     , { day = fromCalendarDate 2019 Time.Apr 25
                                       , meals = []
                                       }
                                     , { day = fromCalendarDate 2019 Time.Apr 26
                                       , meals = []
                                       }
                                     , { day = fromCalendarDate 2019 Time.Apr 27
                                       , meals = []
                                       }
                                     ]
                          got = Result.map Cal.entries <| Cal.addMealPlan bm start_cal
                      in Exp.equal got <| Ok expected
              ]
        , describe "oneWeek"
            [ test "sunday 3 3" <|
                  \_ -> let cal = Cal.forWeeks (fromCalendarDate 2020 Time.Jun 10) Time.Sun 3 3
                            expected = [Sun, Mon, Tue, Wed, Thu, Fri, Sat]
                        in Exp.equal (Cal.oneWeek cal) expected
            , test "monday 1 1" <|
                \_ -> let cal = Cal.forWeeks (fromCalendarDate 2020 Time.Jun 15) Time.Mon 1 1
                          expected = [Mon, Tue, Wed, Thu, Fri, Sat, Sun]
                      in Exp.equal (Cal.oneWeek cal) expected
            ]
        , describe "monthAnchors"
            [ test "no anchor" <|
                  \_ -> let cal = Cal.forWeeks (fromCalendarDate 2020 Time.Jun 17) Time.Sun 1 1
                        in Exp.equal (Cal.monthAnchors cal) []
            , test "one anchor" <|
                \_ -> let cal = Cal.forWeeks (fromCalendarDate 2020 Time.Jun 29) Time.Sun 1 1
                      in Exp.equal (Cal.monthAnchors cal) [{ year = 2020, month = Time.Jul }]
            , test "two anchors" <|
                \_ -> let cal = Cal.forWeeks (fromCalendarDate 2020 Time.Jun 17) Time.Sun 3 3
                      in Exp.equal (Cal.monthAnchors cal) [ { year = 2020, month = Time.Jun }
                                                          , { year = 2020, month = Time.Jul }
                                                          ]
            , test "boundary" <|
                \_ -> let cal = Cal.forWeeks (fromCalendarDate 2020 Time.Jun 4) Time.Mon 1 1
                      in Exp.all
                         [ \() -> Exp.equal
                                  (Cal.startAndEnd cal)
                                  (fromCalendarDate 2020 Time.Jun 1, fromCalendarDate 2020 Time.Jun 8)
                         , \() -> Exp.equal (Cal.monthAnchors cal) [{ year = 2020, month = Time.Jun }]
                         ] ()
            ]
        , describe "extend"
            [ test "+1" <|
                  \_ -> let cal = Cal.forWeeks (calDate 2020 Jul 1) Sun 1 1
                            got = Cal.extend 1 cal
                            exp_period = (calDate 2020 Jun 28, calDate 2020 Jul 12)
                            exp_ex_start = calDate 2020 Jul 5
                            exp_ex_end = calDate 2020 Jul 12
                        in Exp.equal (periodT got) (exp_period, exp_ex_start, exp_ex_end)
            , test "+2" <|
                \_ -> let cal = Cal.forWeeks (calDate 2020 Jul 1) Sun 1 1
                          got = Cal.extend 2 cal
                          exp_period = (calDate 2020 Jun 28, calDate 2020 Jul 19)
                          exp_ex_start = calDate 2020 Jul 5
                          exp_ex_end = calDate 2020 Jul 19
                      in Exp.equal (periodT got) (exp_period, exp_ex_start, exp_ex_end)
            , test "-1" <|
                \_ -> let cal = Cal.forWeeks (calDate 2020 Jul 1) Sun 1 1
                          got = Cal.extend (-1) cal
                          exp_period = (calDate 2020 Jun 21, calDate 2020 Jul 5)
                          exp_ex_start = calDate 2020 Jun 21
                          exp_ex_end = calDate 2020 Jun 28
                      in Exp.equal (periodT got) (exp_period, exp_ex_start, exp_ex_end)
            , test "-2" <|
                \_ -> let cal = Cal.forWeeks (calDate 2020 Jul 1) Sun 1 1
                          got = Cal.extend (-2) cal
                          exp_period = (calDate 2020 Jun 14, calDate 2020 Jul 5)
                          exp_ex_start = calDate 2020 Jun 14
                          exp_ex_end = calDate 2020 Jun 28
                      in Exp.equal (periodT got) (exp_period, exp_ex_start, exp_ex_end)
            , test "0" <|
                \_ -> let cal = Cal.forWeeks (calDate 2020 Jul 1) Sun 1 1
                          got = Cal.extend 0 cal
                          exp_period = (calDate 2020 Jun 28, calDate 2020 Jul 5)
                          exp_ex_start = calDate 2020 Jun 28
                          exp_ex_end = calDate 2020 Jun 28
                      in Exp.equal (periodT got) (exp_period, exp_ex_start, exp_ex_end)
            ]
        ]
