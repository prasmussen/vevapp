module Vevapp.ResultRow exposing (ResultRow, fromViewModel)

import Vevapp.NumberFormat as NumberFormat
import Vevapp.Texts as Texts
import Vevapp.ViewModel as ViewModel


type alias ResultRow =
    { key : Texts.Text
    , value : String
    }


fromViewModel : ViewModel.ViewModel -> List ResultRow
fromViewModel data =
    [ { key = Texts.AmountBeforeDiscountT
      , value = NumberFormat.formatAmount data.amountBeforeDiscount
      }
    , { key = Texts.AmountAfterDiscountT
      , value = NumberFormat.formatAmount data.amountAfterDiscount
      }
    , { key = Texts.DiscountAmountT
      , value = NumberFormat.formatAmount data.discountAmount
      }
    , { key = Texts.DiscountRateT
      , value = NumberFormat.formatAmount data.discountRate
      }
    ]
