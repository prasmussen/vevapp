module Main exposing (main)

import Browser
import Browser.Extra as BrowserExtra
import Browser.Navigation as Nav
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import Random
import Task exposing (Task)
import Time
import Url
import Vevapp.Moment as Moment exposing (Moment)


main =
    BrowserExtra.applicationWithTask
        { init = init
        , loadingView = loadingView
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = ClickedLink
        , onUrlChange = UrlChange
        }


type alias Model =
    { time : Time.Posix
    , title : String
    , when : String
    }


type Msg
    = ClickedLink Browser.UrlRequest
    | UrlChange Url.Url
    | SetTitle String
    | SetWhen String


init : () -> Url.Url -> Nav.Key -> ( Task Never Model, Cmd Msg )
init flags url navKey =
    let
        task =
            Task.map toModel Time.now

        toModel time =
            { time = time
            , title = ""
            , when = ""
            }
    in
    ( task, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedLink urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.load (Url.toString url) )

                Browser.External url ->
                    ( model, Nav.load url )

        UrlChange url ->
            ( model, Cmd.none )

        SetTitle title ->
            ( { model | title = title }, Cmd.none )

        SetWhen when ->
            let
                _ =
                    Moment.parse when
                        |> Debug.log "moment"
            in
            ( { model | when = when }, Cmd.none )


pageTitle : String
pageTitle =
    "Reminders"


loadingView : Browser.Document Msg
loadingView =
    { title = pageTitle
    , body = []
    }


view : Model -> Browser.Document Msg
view model =
    { title = pageTitle
    , body = [ content model ]
    }


content : Model -> Html Msg
content model =
    let
        rows =
            [ Element.row [ Element.width Element.fill, Element.spacing 20 ] [ titleInput model ]
            , Element.row [ Element.width Element.fill, Element.spacing 20 ] [ whenInput model ]
            ]

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


titleInput : Model -> Element Msg
titleInput model =
    let
        label =
            Input.labelAbove [ Font.bold, Font.size 14 ] (Element.text "Title")
    in
    Input.text
        [ borderColor
        , Font.size 20
        , Element.spacing 10
        ]
        { onChange = SetTitle
        , text = model.title
        , placeholder = Nothing
        , label = label
        }


whenInput : Model -> Element Msg
whenInput model =
    let
        label =
            Input.labelAbove [ Font.bold, Font.size 14 ] (Element.text "When")
    in
    Input.text
        [ borderColor
        , Font.size 20
        , Element.spacing 10
        ]
        { onChange = SetWhen
        , text = model.when
        , placeholder = Nothing
        , label = label
        }


borderColor : Element.Attribute Msg
borderColor =
    Border.color (Element.rgb255 211 214 219)
