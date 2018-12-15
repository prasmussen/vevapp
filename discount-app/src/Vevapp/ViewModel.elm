module Vevapp.ViewModel exposing (ViewModel, new)

import Vevapp.Amount as Amount
import Vevapp.AmountType as AmountType
import Vevapp.NumberFormat as NumberFormat
import Vevapp.Rate as Rate
import Vevapp.Texts as Texts


type alias ViewModel =
    { amountBeforeDiscount : Float
    , amountAfterDiscount : Float
    , discountAmount : Float
    , discountRate : Float
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
            Err Texts.EnterDiscountToSeeResultT

        ( Nothing, _ ) ->
            Err Texts.EnterValidAmountT

        ( _, Nothing ) ->
            Err Texts.EnterValidDiscountT

        ( Just (Amount.AmountValue value), Just (Rate.RateValue rateValue) ) ->
            let
                rate =
                    1 - (rateValue / 100)

                amountBeforeDiscount =
                    case model.amountType of
                        AmountType.BeforeDiscount ->
                            value

                        AmountType.AfterDiscount ->
                            value / rate

                amountAfterDiscount =
                    case model.amountType of
                        AmountType.BeforeDiscount ->
                            value * rate

                        AmountType.AfterDiscount ->
                            value

                discountAmount =
                    abs (amountBeforeDiscount - amountAfterDiscount)

                operator =
                    case model.amountType of
                        AmountType.BeforeDiscount ->
                            "x"

                        AmountType.AfterDiscount ->
                            "/"

                resultAmount =
                    case model.amountType of
                        AmountType.BeforeDiscount ->
                            amountAfterDiscount

                        AmountType.AfterDiscount ->
                            amountBeforeDiscount

                calculation =
                    String.concat
                        [ NumberFormat.formatNumber value
                        , " "
                        , operator
                        , " (1 - "
                        , NumberFormat.formatNumber rateValue
                        , " / 100) = "
                        , NumberFormat.formatNumber resultAmount
                        ]
            in
            Ok
                { amountBeforeDiscount = amountBeforeDiscount
                , amountAfterDiscount = amountAfterDiscount
                , discountAmount = discountAmount
                , discountRate = rateValue
                , calculation = calculation
                }
