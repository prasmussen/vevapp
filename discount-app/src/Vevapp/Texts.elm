module Vevapp.Texts exposing (Text(..), toElement, toString)

import Element exposing (Element)


type Text
    = EnterAmountToSeeResultT
    | CalculationT
    | AmountT
    | AmountTypeT
    | EnterValidAmountT
    | DiscountCalculatorT
    | DiscountRateT
    | BeforeDiscountT
    | AfterDiscountT
    | AmountBeforeDiscountT
    | AmountAfterDiscountT
    | EnterDiscountToSeeResultT
    | EnterValidDiscountT
    | DiscountAmountT
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

                DiscountCalculatorT ->
                    { en_us = "Discount Calculator"
                    }

                DiscountRateT ->
                    { en_us = "Discount rate"
                    }

                BeforeDiscountT ->
                    { en_us = "Before discount"
                    }

                AfterDiscountT ->
                    { en_us = "After discount"
                    }

                AmountBeforeDiscountT ->
                    { en_us = "Amount before discount"
                    }

                AmountAfterDiscountT ->
                    { en_us = "Amount after discount"
                    }

                EnterDiscountToSeeResultT ->
                    { en_us = "Enter a discount rate to see the result"
                    }

                EnterValidDiscountT ->
                    { en_us = "Enter a valid discount rate"
                    }

                DiscountAmountT ->
                    { en_us = "Discount amount"
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
