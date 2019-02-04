module Vevapp.Query exposing (Query, buildQuery, parseQuery)

import Dict
import Url
import Url.Builder as Builder
import Url.Parser as Parser
import Url.Parser.Query as QueryParser
import Vevapp.Constants as Const
import Vevapp.Dictionary as Dictionary
import Vevapp.QueryType as QueryType


type alias Query =
    { queryType : Maybe QueryType.QueryType
    , queryString : Maybe String
    , dictionary : Maybe Dictionary.Dictionary
    }


type alias PartialModel a =
    { a
        | queryType : QueryType.QueryType
        , queryString : String
        , dictionary : Dictionary.Dictionary
    }


parseQuery : Url.Url -> Maybe Query
parseQuery url =
    let
        queryTypeDict =
            Dict.fromList
                [ ( QueryType.toString QueryType.Prefix, QueryType.Prefix )
                , ( QueryType.toString QueryType.Suffix, QueryType.Suffix )
                , ( QueryType.toString QueryType.Regex, QueryType.Regex )
                ]

        dictionaryDict =
            Dict.fromList
                [ ( Dictionary.toString Dictionary.NO_UK, Dictionary.NO_UK )
                , ( Dictionary.toString Dictionary.NO_NO, Dictionary.NO_NO )
                , ( Dictionary.toString Dictionary.NO_DE, Dictionary.NO_DE )
                , ( Dictionary.toString Dictionary.NO_ME, Dictionary.NO_ME )
                , ( Dictionary.toString Dictionary.UK_NO, Dictionary.UK_NO )
                , ( Dictionary.toString Dictionary.UK_UK, Dictionary.UK_UK )
                , ( Dictionary.toString Dictionary.UK_FR, Dictionary.UK_FR )
                , ( Dictionary.toString Dictionary.UK_SE, Dictionary.UK_SE )
                , ( Dictionary.toString Dictionary.UK_ES, Dictionary.UK_ES )
                , ( Dictionary.toString Dictionary.SE_UK, Dictionary.SE_UK )
                , ( Dictionary.toString Dictionary.DE_NO, Dictionary.DE_NO )
                , ( Dictionary.toString Dictionary.FR_UK, Dictionary.FR_UK )
                , ( Dictionary.toString Dictionary.ES_UK, Dictionary.ES_UK )
                ]

        parser =
            QueryParser.map3 Query
                (QueryParser.enum Const.queryType queryTypeDict)
                (QueryParser.string Const.queryString)
                (QueryParser.enum Const.dictionary dictionaryDict)
    in
    Parser.parse (Parser.query parser) url


buildQuery : PartialModel a -> String
buildQuery model =
    Builder.toQuery
        [ Builder.string Const.queryType (QueryType.toString model.queryType)
        , Builder.string Const.queryString model.queryString
        , Builder.string Const.dictionary (Dictionary.toString model.dictionary)
        ]
