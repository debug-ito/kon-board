module CalEntrySpec exposing
    ( suite
    )

{-| Unit tests -}

import Expect as Exp
import Date exposing (fromCalendarDate)
import Test exposing (Test, describe, test)
import Time

import Bridge exposing (BMealPlan)
import CalEntry
import MealPhase exposing (MealPhase(..))

suite : Test
suite =
    describe "CalEntry"
        [ describe "merge"
              [ test "merge to empty" <|
                    \_ ->
                    let input = { year = 2019, month = 4, day = 20
                                , phase = "lunch"
                                , recipe_summary = {id = "foo", name = "foo name"}
                                }
                        expected = Ok <|
                                   [ { day = fromCalendarDate 2019 Time.Apr 20
                                     , phase = Lunch
                                     , recipeSummary = Just {id = "foo", name = "foo name"}
                                     }
                                   ]
                        got = CalEntry.merge input []
                    in Exp.equal got expected
              , test "merge to single match" <|
                  \_ ->
                      let bm = { year = 2019, month = 4, day = 20
                               , phase = "lunch"
                               , recipe_summary = {id = "foo", name = "foo name"}
                               }
                          cals = [ { day = fromCalendarDate 2019 Time.Apr 15
                                   , phase = Lunch
                                   , recipeSummary = Nothing
                                   }
                                 , { day = fromCalendarDate 2019 Time.Apr 15
                                   , phase = Dinner
                                   , recipeSummary = Nothing
                                   }
                                 , { day = fromCalendarDate 2019 Time.Apr 16
                                   , phase = Lunch
                                   , recipeSummary = Nothing
                                   }
                                 , { day = fromCalendarDate 2019 Time.Apr 16
                                   , phase = Dinner
                                   , recipeSummary = Nothing
                                   }
                                 , { day = fromCalendarDate 2019 Time.Apr 20
                                   , phase = Lunch
                                   , recipeSummary = Nothing
                                   }
                                 , { day = fromCalendarDate 2019 Time.Apr 20
                                   , phase = Dinner
                                   , recipeSummary = Nothing
                                   }
                                 , { day = fromCalendarDate 2019 Time.Apr 21
                                   , phase = Lunch
                                   , recipeSummary = Nothing
                                   }
                                 , { day = fromCalendarDate 2019 Time.Apr 21
                                   , phase = Dinner
                                   , recipeSummary = Nothing
                                   }
                                 ]
                          expected = [ { day = fromCalendarDate 2019 Time.Apr 15
                                       , phase = Lunch
                                       , recipeSummary = Nothing
                                       }
                                     , { day = fromCalendarDate 2019 Time.Apr 15
                                       , phase = Dinner
                                       , recipeSummary = Nothing
                                       }
                                     , { day = fromCalendarDate 2019 Time.Apr 16
                                       , phase = Lunch
                                       , recipeSummary = Nothing
                                       }
                                     , { day = fromCalendarDate 2019 Time.Apr 16
                                       , phase = Dinner
                                       , recipeSummary = Nothing
                                       }
                                     , { day = fromCalendarDate 2019 Time.Apr 20
                                       , phase = Lunch
                                       , recipeSummary = Just {id = "foo", name = "foo name"}
                                       }
                                     , { day = fromCalendarDate 2019 Time.Apr 20
                                       , phase = Dinner
                                       , recipeSummary = Nothing
                                       }
                                     , { day = fromCalendarDate 2019 Time.Apr 21
                                       , phase = Lunch
                                       , recipeSummary = Nothing
                                       }
                                     , { day = fromCalendarDate 2019 Time.Apr 21
                                       , phase = Dinner
                                       , recipeSummary = Nothing
                                       }
                                     ]
                          got = CalEntry.merge bm cals
                      in Exp.equal got <| Ok expected
              ]
        ]
