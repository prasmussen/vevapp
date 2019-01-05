module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Cons
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes as Attributes
import Url
import Vevapp.Command as Command
import Vevapp.Dictionary as Dictionary
import Vevapp.Language as Language
import Vevapp.Query as Query
import Vevapp.QueryType as QueryType
import Vevapp.Texts as Texts


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
    { queryType : QueryType.QueryType
    , queryString : String
    , languageDictPair : Dictionary.LanguageDictPair
    , dictionary : Dictionary.Dictionary
    , navKey : Nav.Key
    }


type Msg
    = SetQueryType QueryType.QueryType
    | SetQueryString String
    | SetLanguageDictPair Dictionary.LanguageDictPair
    | SetDictionary Dictionary.Dictionary
    | ClickedLink Browser.UrlRequest
    | UrlChange Url.Url


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        defaultDict =
            Dictionary.NO_UK

        model =
            { queryType = QueryType.Prefix
            , queryString = ""
            , languageDictPair = Dictionary.dictToLanguageDictPair defaultDict
            , dictionary = defaultDict
            , navKey = navKey
            }

        modelWithQuery query =
            let
                dictionary =
                    Maybe.withDefault model.dictionary query.dictionary

                languageDictPair =
                    Dictionary.dictToLanguageDictPair dictionary
            in
            { model
                | queryType =
                    Maybe.withDefault model.queryType query.queryType
                , queryString =
                    Maybe.withDefault model.queryString query.queryString
                , dictionary = dictionary
                , languageDictPair = languageDictPair
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
        SetQueryType queryType ->
            ( { model | queryType = queryType }, Cmd.none )
                |> Command.chain updateUrl

        SetQueryString queryString ->
            ( { model | queryString = queryString }, Cmd.none )
                |> Command.chain updateUrl

        SetLanguageDictPair languageDictPair ->
            let
                dictionary =
                    Dictionary.dictFromLanguageDictPair languageDictPair
            in
            ( { model | languageDictPair = languageDictPair, dictionary = dictionary }, Cmd.none )
                |> Command.chain updateUrl

        SetDictionary dictionary ->
            let
                languageDictPair =
                    Dictionary.dictToLanguageDictPair dictionary
            in
            ( { model | languageDictPair = languageDictPair, dictionary = dictionary }, Cmd.none )
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
    { title = Texts.toString Texts.DictionaryT
    , body = [ content model ]
    }


content : Model -> Html Msg
content model =
    let
        rows =
            [ Element.row [ Element.width Element.fill ] [ fromLanguageToggle model ]
            , Element.row [ Element.width Element.fill ] [ toLanguageToggle model ]
            , Element.row [ Element.width Element.fill ] [ queryTypeToggle model ]
            , Element.row [ Element.width Element.fill, Element.spacing 20 ] [ queryInput model ]
            ]

        column =
            Element.column
                [ Element.width (Element.maximum 1200 Element.fill)
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


queryInput : Model -> Element Msg
queryInput model =
    let
        label =
            Input.labelAbove [ Font.bold, Font.size 14 ] (Texts.toElement Texts.QueryT)
    in
    Input.text
        [ borderColor
        , Font.size 20
        , Element.spacing 10
        ]
        { onChange = SetQueryString
        , text = model.queryString
        , placeholder = Nothing
        , label = label
        }


queryTypeToggle : Model -> Element Msg
queryTypeToggle model =
    let
        optionAttributes : QueryType.QueryType -> List (Element.Attribute Msg)
        optionAttributes queryType =
            let
                isSelected =
                    queryType == model.queryType

                borderAttr =
                    if queryType == QueryType.Prefix then
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
                    , Events.onClick (SetQueryType queryType)
                    ]
            in
            if isSelected then
                sharedAttrs
                    ++ [ Background.color (Element.rgb255 31 200 219)
                       , Font.color (Element.rgb255 255 255 255)
                       ]

            else
                sharedAttrs
                    ++ [ Background.color (Element.rgb255 255 255 255)
                       , Font.color (Element.rgb255 0 0 0)
                       ]

        prefixOption =
            Element.el
                (optionAttributes QueryType.Prefix)
                (Element.el [ Element.centerX, Element.centerY ] (Texts.toElement Texts.PrefixT))

        suffixOption =
            Element.el
                (optionAttributes QueryType.Suffix)
                (Element.el [ Element.centerX, Element.centerY ] (Texts.toElement Texts.SuffixT))

        regexOption =
            Element.el
                (optionAttributes QueryType.Regex)
                (Element.el [ Element.centerX, Element.centerY ] (Texts.toElement Texts.RegexT))
    in
    Element.column [ Element.width Element.fill, Element.spacing 10 ]
        [ Element.el [ Font.bold ] (Texts.toElement Texts.QueryTypeT)
        , Element.row [ Element.width Element.fill, Element.spacing 0 ] [ prefixOption, suffixOption, regexOption ]
        ]


fromLanguageToggle : Model -> Element Msg
fromLanguageToggle model =
    let
        optionAttributes : Int -> Dictionary.LanguageDictPair -> List (Element.Attribute Msg)
        optionAttributes index languageDictPair =
            let
                isSelected =
                    languageDictPair == model.languageDictPair

                borderAttr =
                    if index == 0 then
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
                    , Events.onClick (SetLanguageDictPair languageDictPair)
                    ]
            in
            if isSelected then
                sharedAttrs
                    ++ [ Background.color (Element.rgb255 31 200 219)
                       , Font.color (Element.rgb255 255 255 255)
                       ]

            else
                sharedAttrs
                    ++ [ Background.color (Element.rgb255 255 255 255)
                       , Font.color (Element.rgb255 0 0 0)
                       ]

        options =
            List.indexedMap toOption Dictionary.languageDictPairs

        toOption index languageDictPair =
            let
                languageName =
                    Language.toFlag languageDictPair.from
            in
            Element.el
                (optionAttributes index languageDictPair)
                (Element.el [ Element.centerX, Element.centerY ] (Element.text languageName))
    in
    Element.column [ Element.width Element.fill, Element.spacing 10 ]
        [ Element.el [ Font.bold ] (Texts.toElement Texts.FromLanguageT)
        , Element.row [ Element.width Element.fill, Element.spacing 0 ] options
        ]


toLanguageToggle : Model -> Element Msg
toLanguageToggle model =
    let
        optionAttributes : Int -> Dictionary.Dictionary -> List (Element.Attribute Msg)
        optionAttributes index dictionary =
            let
                isSelected =
                    dictionary == model.dictionary

                borderAttr =
                    if index == 0 then
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
                    , Events.onClick (SetDictionary dictionary)
                    ]
            in
            if isSelected then
                sharedAttrs
                    ++ [ Background.color (Element.rgb255 31 200 219)
                       , Font.color (Element.rgb255 255 255 255)
                       ]

            else
                sharedAttrs
                    ++ [ Background.color (Element.rgb255 255 255 255)
                       , Font.color (Element.rgb255 0 0 0)
                       ]

        options =
            List.indexedMap toOption (Cons.toList model.languageDictPair.to)

        toOption index dictionary =
            let
                languageName =
                    Dictionary.toLanguagePair dictionary
                        |> Tuple.second
                        |> Language.toFlag
            in
            Element.el
                (optionAttributes index dictionary)
                (Element.el [ Element.centerX, Element.centerY ] (Element.text languageName))
    in
    Element.column [ Element.width Element.fill, Element.spacing 10 ]
        [ Element.el [ Font.bold ] (Texts.toElement Texts.ToLanguageT)
        , Element.row [ Element.width Element.fill, Element.spacing 0 ] options
        ]


borderColor : Element.Attribute Msg
borderColor =
    Border.color (Element.rgb255 211 214 219)


updateUrl : Model -> Cmd Msg
updateUrl model =
    Nav.replaceUrl model.navKey (Query.buildQuery model)
