module MealPhase exposing
    ( MealPhase(..)
    , toString
    , parseString
    )

{-| MealPhase type. -}

{-| Phase of a meal.
-}
type MealPhase = Breakfast
               | Lunch
               | Dinner
               | MealOther String

toString : MealPhase -> String
toString mp =
    case mp of
        Breakfast -> "朝"
        Lunch -> "昼"
        Dinner -> "夜"
        MealOther s -> s

parseString : String -> Result String MealPhase
parseString s =
    case s of
        "breakfast" -> Ok Breakfast
        "lunch" -> Ok Lunch
        "dinner" -> Ok Dinner
        _ -> Err ("Unknown MealPhase: " ++ s) -- TODO: parser for MealOther
