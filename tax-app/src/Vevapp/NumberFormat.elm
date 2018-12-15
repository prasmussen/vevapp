module Vevapp.NumberFormat exposing (formatAmount, formatNumber, formatSeparator)

import Numeral


formatSeparator : String -> String
formatSeparator str =
    String.replace "," "." str


formatAmount : Float -> String
formatAmount n =
    Numeral.format "0,0.00" n


formatNumber : Float -> String
formatNumber n =
    Numeral.format "0.00" n
