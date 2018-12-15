module Vevapp.Texts exposing (Text(..), toElement, toString)

import Element exposing (Element)


type Text
    = EnterAmountToSeeResultT
    | CalculationT
    | AmountT
    | AmountTypeT
    | EnterValidAmountT
    | TaxCalculatorT
    | TaxRateT
    | IncludingTaxT
    | ExcludingTaxT
    | AmountIncludingTaxT
    | AmountExcludingTaxT
    | EnterTaxRateToSeeResultT
    | EnterValidTaxRateT
    | TaxAmountT
    | ResultT


toString : Text -> String
toString text =
    let
        translations =
            case text of
                EnterAmountToSeeResultT ->
                    { en_us = "Enter an amount to see the result"
                    }

                CalculationT ->
                    { en_us = "Calculation"
                    }

                AmountT ->
                    { en_us = "Amount"
                    }

                AmountTypeT ->
                    { en_us = "Amount type"
                    }

                EnterValidAmountT ->
                    { en_us = "Enter a valid amount"
                    }

                TaxCalculatorT ->
                    { en_us = "Tax Calculator"
                    }

                TaxRateT ->
                    { en_us = "Tax rate"
                    }

                IncludingTaxT ->
                    { en_us = "Including tax"
                    }

                ExcludingTaxT ->
                    { en_us = "Excluding tax"
                    }

                AmountIncludingTaxT ->
                    { en_us = "Amount including tax"
                    }

                AmountExcludingTaxT ->
                    { en_us = "Amount excluding tax"
                    }

                EnterTaxRateToSeeResultT ->
                    { en_us = "Enter a tax rate to see the result"
                    }

                EnterValidTaxRateT ->
                    { en_us = "Enter a valid tax rate"
                    }

                TaxAmountT ->
                    { en_us = "Tax amount"
                    }

                ResultT ->
                    { en_us = "Result"
                    }
    in
    translations.en_us


toElement : Text -> Element msg
toElement text =
    text
        |> toString
        |> Element.text
