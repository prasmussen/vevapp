module Json.Decode.Extra exposing (maybe)

import Json.Decode as JD


maybe : JD.Decoder a -> JD.Decoder (Maybe a)
maybe decoder =
    JD.oneOf
        [ JD.null Nothing
        , JD.map Just decoder
        ]
