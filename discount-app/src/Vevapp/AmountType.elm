module Vevapp.AmountType exposing (AmountType(..), toString)


type AmountType
    = BeforeDiscount
    | AfterDiscount


toString : AmountType -> String
toString amountType =
    case amountType of
        BeforeDiscount ->
            "beforeDiscount"

        AfterDiscount ->
            "afterDiscount"
