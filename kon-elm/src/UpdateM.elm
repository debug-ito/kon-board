module UpdateM exposing
    ( UpdateM
    , append
    , concat
    , run
    , mapModel
    , mapMsg
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

mapModel : (model1 -> model2) -> (model2 -> Maybe model1) -> UpdateM model1 msg -> UpdateM model2 msg
mapModel f g orig model2 =
    case g model2 of
        Nothing -> (model2, [])
        Just model1 -> Tuple.mapFirst f <| orig model1

mapMsg : (msg1 -> msg2) -> UpdateM model msg1 -> UpdateM model msg2
mapMsg f orig model = Tuple.mapSecond (List.map (Cmd.map f)) <| orig model
