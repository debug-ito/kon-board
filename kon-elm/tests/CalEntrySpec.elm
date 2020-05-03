module CalEntrySpec exposing
    ( suite
    )

{-| Unit tests -}

import Expect as Exp
import Date exposing (fromCalendarDate)
import String
import Test exposing (Test, describe, test)
import Time

import Bridge exposing (BMealPlan)
import CalEntry
import MealPhase exposing (MealPhase(..))

suite : Test
suite =
    describe "CalEntry"
        [ describe "addMealPlan"
              [ test "add to empty" <|
                    \_ ->
                    let input = { year = 2019, month = 4, day = 20
                                , phase = "lunch"
                                , recipes = [{id = "foo", name = "foo name"}]
                                }
                        got = CalEntry.addMealPlan input []
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
                          cals = [ { day = fromCalendarDate 2019 Time.Apr 15
                                   , meals = []
                                   }
                                 , { day = fromCalendarDate 2019 Time.Apr 16
                                   , meals = []
                                   }
                                 , { day = fromCalendarDate 2019 Time.Apr 20
                                   , meals = []
                                   }
                                 , { day = fromCalendarDate 2019 Time.Apr 21
                                   , meals = []
                                   }
                                 ]
                          expected = [ { day = fromCalendarDate 2019 Time.Apr 15
                                       , meals = []
                                       }
                                     , { day = fromCalendarDate 2019 Time.Apr 16
                                       , meals = []
                                       }
                                     , { day = fromCalendarDate 2019 Time.Apr 20
                                       , meals = [{phase = Lunch, recipes = [{id = "foo", name = "foo name"}]}]
                                       }
                                     , { day = fromCalendarDate 2019 Time.Apr 21
                                       , meals = []
                                       }
                                     ]
                          got = CalEntry.addMealPlan bm cals
                      in Exp.equal got <| Ok expected
              ]
        ]
