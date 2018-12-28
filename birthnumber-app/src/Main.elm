module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Date
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Url
import Vevapp.BirthNumber as BirthNumber
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
    { birthNumber : String
    , currentDate : Date.Date
    , navKey : Nav.Key
    }


type Msg
    = SetBirthNumber String
    | ClickedLink Browser.UrlRequest
    | UrlChange Url.Url


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        model =
            { birthNumber = "08109012069"
            , navKey = navKey

            -- TODO: get real date
            , currentDate = Date.fromOrdinalDate 2018 1
            }
    in
    ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetBirthNumber birthNumber ->
            ( { model | birthNumber = birthNumber }, Cmd.none )

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
    { title = Texts.toString Texts.BirthNumberValidatorT
    , body = [ content model ]
    }


content : Model -> Html Msg
content model =
    let
        viewModelResult =
            ViewModel.new model

        resultRows =
            case viewModelResult of
                ViewModel.ParseFailure failure ->
                    case failure of
                        BirthNumber.InvalidInput ->
                            [ Element.row [ Element.width Element.fill, Element.spacing 20 ] [ Element.text "Only numbers are supported" ]
                            ]

                        BirthNumber.WrongInputLength ->
                            [ Element.row [ Element.width Element.fill, Element.spacing 20 ] [ Element.text "Please enter 9 or 11 digits" ]
                            ]

                ViewModel.InvalidNineDigitBirthNumber ->
                    [ Element.row [ Element.width Element.fill, Element.spacing 20 ] [ Element.text "No check digits exist for this 9 digit birth number." ]
                    ]

                ViewModel.WrongCheckDigits parsedBirthNumber ->
                    [ Element.row [ Element.width Element.fill, Element.spacing 20 ] [ Element.text "Check digits are wrong" ]
                    ]

                ViewModel.WrongBirthDate parsedBirthNumber ->
                    [ Element.row [ Element.width Element.fill, Element.spacing 20 ] [ Element.text "Not a valid birthdate" ]
                    ]

                ViewModel.ValidBirthNumber birthNumber ->
                    [ Element.row [ Element.width Element.fill, Element.spacing 20 ] [ resultTable (ResultRow.fromBirthNumber birthNumber) ]
                    ]

        rows =
            [ Element.row [ Element.width Element.fill, Element.spacing 20 ] [ birthNumberInput model ]
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


birthNumberInput : Model -> Element Msg
birthNumberInput model =
    let
        label =
            Input.labelAbove [ Font.bold, Font.size 14 ] (Texts.toElement labelText)

        labelText =
            Texts.NorwegianBirthNumberT
    in
    Input.text
        [ borderColor
        , Font.size 20
        , Element.spacing 10
        ]
        { onChange = SetBirthNumber
        , text = model.birthNumber
        , placeholder = Nothing
        , label = label
        }


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
