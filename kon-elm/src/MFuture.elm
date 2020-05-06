module MFuture exposing
    ( MFuture(..)
    )

{- | Futures (aka Promises) -}

{- | MFuture is a Future that (1) can be 'NotStarted' state in which
     the task behind it is not started yet, and (2) will always
     succeed.
-}
type MFuture a = NotStarted
               | Running
               | Finished a


