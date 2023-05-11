module UpdateM exposing
    ( UpdateM
    , append
    , run
    )

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

run : UpdateM model msg -> model -> (model, Cmd msg)
run m model =
    let (newModel, cmds) = m model
    in (newModel, Cmd.batch cmds)
