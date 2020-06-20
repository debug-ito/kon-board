module CalSpySpec exposing (suite)

import Expect as Exp
import Test exposing (Test, describe, test)
import Time exposing (Month(..))

import Calendar exposing (MonthAnchor)
import CalSpy exposing (currentMonthAnchor, testMakeCalLayout)


todayA : MonthAnchor
todayA = { year = 2020, month = May }

suite : Test
suite =
    describe "CalSpy"
    [ describe "currentMonthAnchor"
      [ test "viewport - today" <|
            \_ -> let cl = testMakeCalLayout 10.0 20.0 []
                      got = currentMonthAnchor todayA cl
                  in Exp.equal got todayA
      , test "today - viewport" <|
            \_ -> let cl = testMakeCalLayout 30.0 20.0 []
                      got = currentMonthAnchor todayA cl
                  in Exp.equal got todayA
      , test "viewport - anchor" <|
          \_ -> let cl = testMakeCalLayout 10.0 30.0 [({ year = 2020, month = Apr }, 20.0)]
                    got = currentMonthAnchor todayA cl
                in Exp.equal got { year = 2020, month = Mar }
      , test "anchor - viewport" <|
          \_ -> let cl = testMakeCalLayout 25.0 30.0 [({ year = 2020, month = Apr }, 20.0)]
                    got = currentMonthAnchor todayA cl
                in Exp.equal got { year = 2020, month = Apr }
      , test "anchor - viewport - anchor" <|
          \_ -> let cl = testMakeCalLayout 25.0 30.0 [({ year = 2020, month = Apr }, 20.0), ({ year = 2020, month = May }, 29.0)]
                    got = currentMonthAnchor todayA cl
                in Exp.equal got { year = 2020, month = Apr }
      , test "anchor - anchor - viewport" <|
          \_ -> let cl = testMakeCalLayout 35.0 30.0 [({ year = 2020, month = Apr }, 20.0), ({ year = 2020, month = May }, 29.0)]
                    got = currentMonthAnchor todayA cl
                in Exp.equal got { year = 2020, month = May }
      , test "anchor - anchor - anchor - viewport - anchor (not sorted)" <|
          \_ -> let cl = testMakeCalLayout 35.0 30.0
                         [ ({ year = 2020, month = Feb }, 20.0)
                         , ({ year = 2020, month = Jan }, 10.0)
                         , ({ year = 2020, month = Apr }, 40.0)
                         , ({ year = 2020, month = Mar }, 30.0)
                         ]
                    got = currentMonthAnchor todayA cl
                in Exp.equal got { year = 2020, month = Mar }
      ]
    ]
