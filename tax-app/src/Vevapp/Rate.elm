module Vevapp.Rate exposing (Rate(..), fromString)

import Vevapp.NumberFormat as NumberFormat


type Rate
    = RateValue Float
    | EmptyRate


fromString : String -> Maybe Rate
fromString str =
    if str == "" then
        Just EmptyRate

    else
        NumberFormat.formatSeparator str
            |> String.toFloat
            |> Maybe.map RateValue
