module Vevapp.User exposing
    ( User
    , decoder
    )

import Json.Decode as Decode


type alias User =
    { email : String
    }


decoder : Decode.Decoder User
decoder =
    Decode.map User
        (Decode.field "email" Decode.string)
