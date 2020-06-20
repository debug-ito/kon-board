module CalSpy exposing
    ( CalLayout
    , getCalLayoutTask
    , setCalendarViewportTask
    , currentMonthAnchor
    , relativeCalendarViewportY
    , todayCellID
    , monthAnchorCellID
        
    , testMakeCalLayout
    )

{- | Spying viewport position relative to the calendar. -}

import Browser.Dom as Dom
import Date exposing (monthToNumber)
import Task
import Task exposing (Task)

import Calendar exposing
    (MonthAnchor, prevMonthAnchor, compareMonthAnchors)

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
currentMonthAnchor : MonthAnchor -> CalLayout -> MonthAnchor
currentMonthAnchor today_anchor (CalLayout cl) =
    let result =
            case sorted_anchors of
                [] -> today_anchor
                (head_a :: rest) -> go head_a rest
        sorted_anchors = List.sortWith (\a b -> compareMonthAnchors a.manchor b.manchor) cl.months
        go head_a rest =
            if cl.viewport.y < head_a.pos.y
            then prevMonthAnchor head_a.manchor
            else case rest of
                     [] -> head_a.manchor
                     (next_a :: next_rest) -> go next_a next_rest
    in result

{- | Get 'MonthAnchorPos' and the viewport position.
-}
getMonthAnchorPosTask : MonthAnchor -> Task String MonthAnchorPos
getMonthAnchorPosTask ma =
    let result = Task.map mkResult <| getElementTask (monthAnchorCellID ma)
        mkResult e = {manchor = ma, pos = {x = e.element.x, y = e.element.y}}
    in result

{- | Task to get 'CalLayout'.
-}
getCalLayoutTask : List MonthAnchor -> Task String CalLayout
getCalLayoutTask mas =
    let result =
            getElementTask todayCellID |> Task.andThen
            ( \today_elem -> Task.sequence (List.map getMonthAnchorPosTask mas) |> Task.andThen
              ( \ma_poss ->
                    Task.succeed
                    <| CalLayout
                       { viewport = { x = today_elem.viewport.x, y = today_elem.viewport.y }
                       , today = { x = today_elem.element.x, y = today_elem.element.y }
                       , months = ma_poss
                       }
              )
            )
    in result

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

{- | Element ID of a cell of month anchor
-}
monthAnchorCellID : MonthAnchor -> String
monthAnchorCellID ma =
    let result = "cal-month-anchor-" ++ year_str ++ month_str
        year_str = String.fromInt ma.year
        month_str = String.padLeft 2 '0' <| String.fromInt <| monthToNumber <| ma.month
    in result

{- | Same as 'Dom.getElement' except that the error is a
human-readable string.
-}
getElementTask : String -> Task String Dom.Element
getElementTask elem_id =
    let toString (Dom.NotFound e) =
            "Cannot find #" ++ elem_id ++ ": " ++ e
    in Task.mapError toString <| Dom.getElement elem_id

{- | (Only for test) Make CalLayout from Y positions.
-}
testMakeCalLayout : Float -> Float -> List (MonthAnchor, Float) -> CalLayout
testMakeCalLayout viewport_y today_y manchors =
    let result = CalLayout
                 { viewport = { x = 0, y = viewport_y }
                 , today = {x = 0, y = today_y}
                 , months = List.map toMAPos manchors
                 }
        toMAPos (ma, ma_y) = { manchor = ma, pos = { x = 0, y = ma_y} }
    in result
