module CalSpy exposing
    ( CalLayout
    , getCalLayoutTask
    , setCalendarViewportTask
    , currentMonthAnchor
    , relativeCalendarViewportY
    , todayCellID
    )

{- | Spying viewport position relative to the calendar. -}

import Browser.Dom as Dom
import Task
import Task exposing (Task)

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

{- | Task to get 'CalLayout'.
-}
getCalLayoutTask : Task String CalLayout
getCalLayoutTask = Debug.todo "TODO: implement it"

{- | Get Y position of the viewport relative to "today" cell.
-}
relativeCalendarViewportY : CalLayout -> Float
relativeCalendarViewportY (CalLayout c) = c.viewport.y - c.today.y

{- | Task to set viewport (y position) relative to the element of
"today".
-}
setCalendarViewportTask : Float -> Task String ()
setCalendarViewportTask rel_y =
    let result =
            getElementTask todayCellID |> Task.andThen
            (\ elem ->
                 let new_x = elem.viewport.x
                     new_y = elem.element.y + rel_y
                 in Dom.setViewport new_x new_y
            )
    in result


{- | Element ID of "today" cell
-}
todayCellID : String
todayCellID = "cal-today-cell"

{- | Same as 'Dom.getElement' except that the error is a
human-readable string.
-}
getElementTask : String -> Task String Dom.Element
getElementTask elem_id =
    let toString (Dom.NotFound e) =
            "Cannot find #" ++ elem_id ++ ": " ++ e
    in Task.mapError toString <| Dom.getElement elem_id
