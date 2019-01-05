module Vevapp.Entry exposing
    ( Entry
    , decoder
    )

import Json.Decode as JD


type alias Entry =
    { word : String
    , translations : List String
    }


decoder : JD.Decoder Entry
decoder =
    JD.map2 Entry
        (JD.field "word" JD.string)
        (JD.field "translations" (JD.list JD.string))
