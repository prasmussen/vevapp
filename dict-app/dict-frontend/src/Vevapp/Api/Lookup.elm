module Vevapp.Api.Lookup exposing (lookup)

import Http
import Json.Decode as JD
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


lookup : (Result Http.Error (List Entry.Entry) -> msg) -> PartialModel a -> Cmd msg
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
    in
    Http.get
        { url = url
        , expect = Http.expectJson toMsg decoder
        }
