module Vevapp.QueryType exposing (QueryType(..), toString)


type QueryType
    = Prefix
    | Suffix
    | Regex


toString : QueryType -> String
toString queryType =
    case queryType of
        Prefix ->
            "prefix"

        Suffix ->
            "suffix"

        Regex ->
            "regex"
