module Vevapp.AmountType exposing (AmountType(..), toString)


type AmountType
    = IncludingTax
    | ExcludingTax


toString : AmountType -> String
toString amountType =
    case amountType of
        IncludingTax ->
            "includingTax"

        ExcludingTax ->
            "exludingTax"
