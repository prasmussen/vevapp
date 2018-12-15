module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes as Attributes
import Url
import Vevapp.Amount as Amount
import Vevapp.AmountType as AmountType
import Vevapp.Command as Command
import Vevapp.Query as Query
import Vevapp.Rate as Rate
import Vevapp.ResultRow as ResultRow
import Vevapp.Texts as Texts
import Vevapp.ViewModel as ViewModel


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = ClickedLink
        , onUrlChange = UrlChange
        }


type alias Model =
    { amountType : AmountType.AmountType
    , amount : String
    , rate : String
    , navKey : Nav.Key
    }


type Msg
    = SetAmountType AmountType.AmountType
    | SetAmount String
    | SetRate String
    | ClickedLink Browser.UrlRequest
    | UrlChange Url.Url


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        model =
            { amountType = AmountType.IncludingTax
            , amount = "100"
            , rate = "25"
            , navKey = navKey
            }

        modelWithQuery query =
            { model
                | amountType =
                    Maybe.withDefault model.amountType query.amountType
                , amount =
                    Maybe.withDefault model.amount query.amount
                , rate =
                    Maybe.withDefault model.rate query.rate
            }
    in
    case Query.parseQuery url of
        Just query ->
            ( modelWithQuery query, Cmd.none )

        Nothing ->
            ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetAmountType amountType ->
            ( { model | amountType = amountType }, Cmd.none )
                |> Command.chain updateUrl

        SetAmount amount ->
            ( { model | amount = amount }, Cmd.none )
                |> Command.chain updateUrl

        SetRate rate ->
            ( { model | rate = rate }, Cmd.none )
                |> Command.chain updateUrl

        ClickedLink urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.load (Url.toString url) )

                Browser.External url ->
                    ( model, Nav.load url )

        UrlChange url ->
            ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = Texts.toString Texts.TaxCalculatorT
    , body = [ content model ]
    }


content : Model -> Html Msg
content model =
    let
        viewModelResult =
            ViewModel.new model

        resultRows =
            case viewModelResult of
                Ok viewModel ->
                    [ Element.row [ Element.width Element.fill, Element.spacing 20 ] [ resultTable (ResultRow.fromViewModel viewModel) ]
                    , Element.row [ Element.width Element.fill, Element.spacing 20 ] [ calculationSection viewModel.calculation ]
                    ]

                Err err ->
                    [ Element.row [ Element.width Element.fill, Element.spacing 20 ] [ Texts.toElement err ]
                    ]

        rows =
            [ Element.row [ Element.width Element.fill ] [ amountTypeToggle model ]
            , Element.row [ Element.width Element.fill, Element.spacing 20 ] [ amountInput model ]
            , Element.row [ Element.width Element.fill, Element.spacing 20 ] [ rateInput model ]
            ]
                ++ resultRows

        column =
            Element.column
                [ Element.width (Element.maximum 800 Element.fill)
                , Element.spacing 20
                , Element.padding 20
                , Element.centerX
                , Font.size 14
                , Background.color (Element.rgb255 245 247 250)
                ]
                rows
    in
    Element.layout
        [ Element.width Element.fill
        , Element.height Element.fill
        , Background.color (Element.rgb255 245 247 250)
        ]
        column


amountInput : Model -> Element Msg
amountInput model =
    let
        label =
            Input.labelAbove [ Font.bold, Font.size 14 ] (Texts.toElement labelText)

        labelText =
            case model.amountType of
                AmountType.IncludingTax ->
                    Texts.AmountIncludingTaxT

                AmountType.ExcludingTax ->
                    Texts.AmountExcludingTaxT
    in
    Input.text
        [ borderColor
        , Font.size 20
        , Element.spacing 10
        , Element.htmlAttribute (Attributes.type_ "number")
        , Element.htmlAttribute (Attributes.step "any")
        ]
        { onChange = SetAmount
        , text = model.amount
        , placeholder = Nothing
        , label = label
        }


rateInput : Model -> Element Msg
rateInput model =
    let
        label =
            Input.labelAbove
                [ Font.bold
                , Font.size 14
                ]
                (Texts.toElement Texts.TaxRateT)
    in
    Input.text
        [ borderColor
        , Font.size 20
        , Element.spacing 10
        , Element.htmlAttribute (Attributes.type_ "number")
        , Element.htmlAttribute (Attributes.step "any")
        ]
        { onChange = SetRate
        , text = model.rate
        , placeholder = Nothing
        , label = label
        }


amountTypeToggle : Model -> Element Msg
amountTypeToggle model =
    let
        optionAttributes : AmountType.AmountType -> List (Element.Attribute Msg)
        optionAttributes amountType =
            let
                isSelected =
                    amountType == model.amountType

                borderAttr =
                    if amountType == AmountType.IncludingTax then
                        Border.widthEach
                            { bottom = 1
                            , right = 1
                            , left = 1
                            , top = 1
                            }

                    else
                        Border.widthEach
                            { bottom = 1
                            , right = 1
                            , left = 0
                            , top = 1
                            }

                sharedAttrs =
                    [ Border.color (Element.rgb255 217 217 217)
                    , borderAttr
                    , Element.width Element.fill
                    , Element.padding 13
                    , Font.size 18
                    , Element.pointer
                    , Events.onClick (SetAmountType amountType)
                    ]
            in
            if isSelected then
                sharedAttrs
                    ++ [ Background.color (Element.rgb255 70 70 70)
                       , Font.color (Element.rgb255 255 255 255)
                       ]

            else
                sharedAttrs
                    ++ [ Background.color (Element.rgb255 255 255 255)
                       , Font.color (Element.rgb255 0 0 0)
                       ]

        includingTaxOption =
            Element.el
                (optionAttributes AmountType.IncludingTax)
                (Element.el [ Element.centerX, Element.centerY ] (Texts.toElement Texts.IncludingTaxT))

        exludingTaxOption =
            Element.el
                (optionAttributes AmountType.ExcludingTax)
                (Element.el [ Element.centerX, Element.centerY ] (Texts.toElement Texts.ExcludingTaxT))
    in
    Element.column [ Element.width Element.fill, Element.spacing 10 ]
        [ Element.el [ Font.bold ] (Texts.toElement Texts.AmountTypeT)
        , Element.row [ Element.width Element.fill, Element.spacing 0 ] [ includingTaxOption, exludingTaxOption ]
        ]


borderColor : Element.Attribute Msg
borderColor =
    Border.color (Element.rgb255 211 214 219)


resultTable : List ResultRow.ResultRow -> Element Msg
resultTable rows =
    let
        rowPadding =
            Element.padding 10

        rowColor index =
            if modBy 2 index == 0 then
                Background.color (Element.rgb255 255 255 255)

            else
                Background.color (Element.rgb255 245 247 250)

        leftColumnView index row =
            Element.el
                [ rowColor index
                , Font.bold
                , Border.widthEach
                    { bottom = 1
                    , left = 1
                    , right = 1
                    , top = 0
                    }
                , borderColor
                , rowPadding
                ]
                (Texts.toElement row.key)

        rightColumnView index row =
            Element.el
                [ rowColor index
                , rowPadding
                , Border.widthEach
                    { bottom = 1
                    , left = 0
                    , right = 1
                    , top = 0
                    }
                , borderColor
                ]
                (Element.text row.value)

        table =
            Element.indexedTable
                [ Border.widthEach
                    { bottom = 0
                    , left = 0
                    , right = 0
                    , top = 1
                    }
                , borderColor
                ]
                { data = rows
                , columns =
                    [ { header = Element.none
                      , width = Element.fill
                      , view = leftColumnView
                      }
                    , { header = Element.none
                      , width = Element.fill
                      , view = rightColumnView
                      }
                    ]
                }
    in
    Element.column [ Element.width Element.fill, Element.spacing 10 ]
        [ Element.el [ Font.bold ] (Texts.toElement Texts.ResultT)
        , table
        ]


calculationSection : String -> Element Msg
calculationSection calcString =
    Element.column [ Element.width Element.fill, Element.spacing 10 ]
        [ Element.el [ Font.bold ] (Texts.toElement Texts.CalculationT)
        , Element.text calcString
        ]


updateUrl : Model -> Cmd Msg
updateUrl model =
    Nav.replaceUrl model.navKey (Query.buildQuery model)
