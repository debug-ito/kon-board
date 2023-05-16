module UpdateM exposing
    ( UpdateM
    , append
    , concat
    , run
    , mapBoth
    )

import Tuple
import List exposing (foldr)

{- | The update monoid. This is the basic signature of application's `update` function.
   This is equivalent to `StateT model (Writer [Cmd msg] ())` in Haskell.
-}
type alias UpdateM model msg = model -> (model, List (Cmd msg))

empty : UpdateM model msg
empty model = (model, [])

append : UpdateM model msg -> UpdateM model msg -> UpdateM model msg
append a b model =
    let (aModel, aCmds) = a model
        (bModel, bCmds) = b aModel
    in (bModel, aCmds ++ bCmds)

concat : List (UpdateM model msg) -> UpdateM model msg
concat = foldr append empty

run : UpdateM model msg -> model -> (model, Cmd msg)
run m model =
    let (newModel, cmds) = m model
    in (newModel, Cmd.batch cmds)

mapBoth : (model1 -> model2) -> (msg1 -> msg2) -> (model1, List (Cmd msg1)) -> (model2, List (Cmd msg2))
mapBoth fModel fMsg = Tuple.mapBoth fModel (\m -> List.map (Cmd.map fMsg))
