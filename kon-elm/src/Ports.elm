port module Ports exposing
    ( portOnScroll
    )

import Json.Decode exposing (Value)

port portOnScroll : (Value -> msg) -> Sub msg
