port module Ports exposing
    ( portOnScroll
    , portSelectElement
    )

import Json.Decode exposing (Value)

port portOnScroll : (Value -> msg) -> Sub msg

port portSelectElement : String -> Cmd msg
