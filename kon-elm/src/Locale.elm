module Locale exposing
    ( Locale(..)
    , LocaleImpl
    , get
    )

{- | Locale support -}

import Date exposing (Date)
import Html exposing (Html)

import MealPhase exposing (MealPhase(..))

{- | Locale symbols.
-}
type Locale = LJaJP

{- | Locale-dependent functions.
-}
type alias LocaleImpl msg =
    { viewDateLong : Date -> Html msg
    , viewDateShort : Date -> Html msg
    , showMealPhase : MealPhase -> String
    , showCalDay : String
    , showIngredients : String
    , showRecipeSteps : String
    , showRecipeReference : String
    }

get : Locale -> LocaleImpl msg
get l =
    case l of
        LJaJP -> localeJaJP

localeJaJP : LocaleImpl msg
localeJaJP =
    let result =
            { viewDateLong = jaViewDateLong
            , viewDateShort = jaViewDateShort
            , showMealPhase = jaShowMealPhase
            , showCalDay = "日付"
            , showIngredients = "材料"
            , showRecipeSteps = "手順"
            , showRecipeReference = "参考"
            }
        jaViewDateLong d = Debug.todo "todo"
        jaViewDateShort d = Debug.todo "todo"
        jaShowMealPhase mp =
            case mp of
                Breakfast -> "朝食"
                Lunch -> "昼食"
                Dinner -> "夕食"
                MealOther s -> s
    in result
