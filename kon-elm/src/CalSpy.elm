module CalSpy exposing
    ( CalLayout
    , currentMonthAnchor
    )

{- | Spying viewport position relative to the calendar. -}

import Calendar exposing (MonthAnchor)

{- | 2D Position.
-}
type alias Pos =
    { x : Float
    , y : Float
    }

{- | Position of 'MonthAnchor'.
-}
type alias MonthAnchorPos =
    { manchor : MonthAnchor
    , pos : Pos
    }

{- | Opaque type of calendar layout.
-}
type CalLayout = CalLayout CL

{- | Private internal of 'CalLayout'
-}
type alias CL =
    { -- | Position of the viewport.
      viewport : Pos
      -- | Position of the cell of today.
    , today : Pos
    , months : List MonthAnchorPos
    }

{- | Get the 'MonthAnchor' corresponding to the current viewport
position.
-}
currentMonthAnchor : CalLayout -> MonthAnchor
currentMonthAnchor = Debug.todo "TODO: implement it"
