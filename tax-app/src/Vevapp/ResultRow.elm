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
    [ { key = Texts.AmountIncludingTaxT
      , value = NumberFormat.formatAmount data.amountIncludingTax
      }
    , { key = Texts.AmountExcludingTaxT
      , value = NumberFormat.formatAmount data.amountExcludingTax
      }
    , { key = Texts.TaxAmountT
      , value = NumberFormat.formatAmount data.taxAmount
      }
    , { key = Texts.TaxRateT
      , value = NumberFormat.formatAmount data.taxRate
      }
    ]
