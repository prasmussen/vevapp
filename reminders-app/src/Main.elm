module Main exposing (main)

import Browser
import Browser.Extra as BrowserExtra
import Browser.Navigation as Nav
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
    }


type Msg
    = ClickedLink Browser.UrlRequest
    | UrlChange Url.Url


init : () -> Url.Url -> Nav.Key -> ( Task Never Model, Cmd Msg )
init flags url navKey =
    let
        task =
            Task.map toModel Time.now

        toModel time =
            { time = time
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


title : String
title =
    "Reminders"


loadingView : Browser.Document Msg
loadingView =
    { title = title
    , body = []
    }


view : Model -> Browser.Document Msg
view model =
    { title = title
    , body = [ Html.text "hello" ]
    }
