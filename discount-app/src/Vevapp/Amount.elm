module Vevapp.Amount exposing (Amount(..), fromString)

import Vevapp.NumberFormat as NumberFormat


type Amount
    = AmountValue Float
    | EmptyAmount


fromString : String -> Maybe Amount
fromString str =
    if str == "" then
        Just EmptyAmount

    else
        NumberFormat.formatSeparator str
            |> String.toFloat
            |> Maybe.map AmountValue
