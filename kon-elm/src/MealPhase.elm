module MealPhase exposing
    ( MealPhase(..)
    , toString
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

