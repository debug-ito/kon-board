module CalendarSpec exposing
    ( suite
    )

{-| Unit tests -}

import Expect as Exp
import Date exposing (fromCalendarDate)
import String
import Test exposing (Test, describe, test)
import Time
import Time exposing (Weekday(..))

import Bridge exposing (BMealPlan)
import Calendar as Cal
import MealPhase exposing (MealPhase(..))

suite : Test
suite =
    describe "Calendar"
        [ describe "addMealPlan"
              [ test "no match" <|
                    \_ ->
                    let input = { year = 2019, month = 4, day = 20
                                , phase = "lunch"
                                , recipes = [{id = "foo", name = "foo name"}]
                                }
                        start_cal = Cal.forWeeks (fromCalendarDate 2019 Time.May 20) Time.Sun 2 2
                        got = Cal.addMealPlan input start_cal
                    in case got of
                           Ok _ -> Exp.fail "the result should fail."
                           Err e -> Exp.true
                                    ("Unexpected error message: " ++ e)
                                    (String.contains "Cannot find CalEntry for 2019-04-20" e)
              , test "add single match" <|
                  \_ ->
                      let bm = { year = 2019, month = 4, day = 20
                               , phase = "lunch"
                               , recipes = [{id = "foo", name = "foo name"}]
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
                                       , meals = [{phase = Lunch, recipes = [{id = "foo", name = "foo name"}]}]
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
        ]
