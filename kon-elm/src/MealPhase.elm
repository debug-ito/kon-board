module MealPhase exposing
    ( MealPhase(..)
    , toString
    , fromString
    )

{- | MealPhase type. -}

{- | Phase of a meal.
-}
type MealPhase = Breakfast
               | Lunch
               | Dinner
               | MealOther String

{- | Encode the MealPhase into String. Note that this is for data
serialization (e.g. for JSON), not for humans. To convert MealPhase
into a human-readable string, use Locale module.
-}
toString : MealPhase -> String
toString mp =
    case mp of
        Breakfast -> "breakfast"
        Lunch -> "lunch"
        Dinner -> "dinner"
        MealOther s -> "@" ++ s

{- | Parse string into MealPhase. This is the dual of toString.
-}
fromString : String -> Result String MealPhase
fromString s =
    case s of
        "breakfast" -> Ok Breakfast
        "lunch" -> Ok Lunch
        "dinner" -> Ok Dinner
        _ ->
            if String.startsWith "@" s
            then Ok <| MealOther <| String.dropLeft 1 s
            else Err ("Unknown MealPhase: " ++ s)
