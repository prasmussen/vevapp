module Vevapp.Api.Lookup exposing (lookup)

import Http
import Json.Decode as JD
import RemoteData
import Vevapp.Dictionary as Dictionary
import Vevapp.Entry as Entry
import Vevapp.Query as Query
import Vevapp.QueryType as QueryType


type alias PartialModel a =
    { a
        | queryType : QueryType.QueryType
        , queryString : String
        , dictionary : Dictionary.Dictionary
    }


lookup : (RemoteData.WebData (List Entry.Entry) -> msg) -> PartialModel a -> Cmd msg
lookup toMsg model =
    let
        queryString =
            Query.buildQuery model

        url =
            String.concat
                [ "/api/lookup"
                , queryString
                ]

        decoder =
            JD.list Entry.decoder

        toWebDataMsg =
            RemoteData.fromResult >> toMsg
    in
    Http.get
        { url = url
        , expect = Http.expectJson toWebDataMsg decoder
        }
