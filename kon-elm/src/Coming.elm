module Coming exposing
    ( Coming(..)
    , hasStarted
    , success
    , isPending
    )

{- | 'Coming' is like a future or promise. Difference from future is
(1) it can be 'NotStarted' state (2) it's not directly backed by
asynchronous process. It's not possible to chain asynchronous
processes.
-}
type Coming e a = NotStarted
                | Pending
                | Failure e
                | Success a

hasStarted : Coming e a -> Bool
hasStarted c =
    case c of
        NotStarted -> False
        _ -> True

success : Coming e a -> Maybe a
success c =
    case c of
        Success a -> Just a
        _ -> Nothing

isPending : Coming e a -> Bool
isPending c =
    case c of
        Pending -> True
        _ -> False
