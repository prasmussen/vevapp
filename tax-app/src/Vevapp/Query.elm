module Vevapp.Query exposing (Query, buildQuery, parseQuery)

import Dict
import Url
import Url.Builder as Builder
import Url.Parser as Parser
import Url.Parser.Query as QueryParser
import Vevapp.AmountType as AmountType
import Vevapp.Constants as Const


type alias Query =
    { amountType : Maybe AmountType.AmountType
    , amount : Maybe String
    , rate : Maybe String
    }


type alias PartialModel a =
    { a
        | amountType : AmountType.AmountType
        , amount : String
        , rate : String
    }


parseQuery : Url.Url -> Maybe Query
parseQuery url =
    let
        amountTypeDict =
            Dict.fromList
                [ ( AmountType.toString AmountType.IncludingTax, AmountType.IncludingTax )
                , ( AmountType.toString AmountType.ExcludingTax, AmountType.ExcludingTax )
                ]

        parser =
            QueryParser.map3 Query
                (QueryParser.enum Const.amountType amountTypeDict)
                (QueryParser.string Const.amount)
                (QueryParser.string Const.rate)
    in
    Parser.parse (Parser.query parser) url


buildQuery : PartialModel a -> String
buildQuery model =
    Builder.toQuery
        [ Builder.string Const.amountType (AmountType.toString model.amountType)
        , Builder.string Const.amount model.amount
        , Builder.string Const.rate model.rate
        ]
