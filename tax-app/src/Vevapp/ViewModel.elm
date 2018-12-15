module Vevapp.ViewModel exposing (ViewModel, new)

import Vevapp.Amount as Amount
import Vevapp.AmountType as AmountType
import Vevapp.NumberFormat as NumberFormat
import Vevapp.Rate as Rate
import Vevapp.Texts as Texts


type alias ViewModel =
    { amountIncludingTax : Float
    , amountExcludingTax : Float
    , taxAmount : Float
    , taxRate : Float
    , calculation : String
    }


type alias PartialModel a =
    { a
        | amountType : AmountType.AmountType
        , amount : String
        , rate : String
    }


new : PartialModel a -> Result Texts.Text ViewModel
new model =
    case ( Amount.fromString model.amount, Rate.fromString model.rate ) of
        ( Just Amount.EmptyAmount, _ ) ->
            Err Texts.EnterAmountToSeeResultT

        ( _, Just Rate.EmptyRate ) ->
            Err Texts.EnterTaxRateToSeeResultT

        ( Nothing, _ ) ->
            Err Texts.EnterValidAmountT

        ( _, Nothing ) ->
            Err Texts.EnterValidTaxRateT

        ( Just (Amount.AmountValue value), Just (Rate.RateValue rateValue) ) ->
            let
                rate =
                    1 + (rateValue / 100)

                amountIncludingTax =
                    case model.amountType of
                        AmountType.IncludingTax ->
                            value

                        AmountType.ExcludingTax ->
                            value * rate

                amountExcludingTax =
                    case model.amountType of
                        AmountType.IncludingTax ->
                            value / rate

                        AmountType.ExcludingTax ->
                            value

                taxAmount =
                    abs (amountIncludingTax - amountExcludingTax)

                operator =
                    case model.amountType of
                        AmountType.IncludingTax ->
                            "/"

                        AmountType.ExcludingTax ->
                            "x"

                resultAmount =
                    case model.amountType of
                        AmountType.IncludingTax ->
                            amountExcludingTax

                        AmountType.ExcludingTax ->
                            amountIncludingTax

                calculation =
                    String.concat
                        [ NumberFormat.formatNumber value
                        , " "
                        , operator
                        , " (1 + "
                        , NumberFormat.formatNumber rateValue
                        , " / 100) = "
                        , NumberFormat.formatNumber resultAmount
                        ]
            in
            Ok
                { amountIncludingTax = amountIncludingTax
                , amountExcludingTax = amountExcludingTax
                , taxAmount = taxAmount
                , taxRate = rateValue
                , calculation = calculation
                }
