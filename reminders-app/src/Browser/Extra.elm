module Browser.Extra exposing
    ( applicationWithDecoder
    , applicationWithTask
    )

import Browser
import Browser.Navigation as Navigation
import Html
import Json.Decode as JD
import Task exposing (Task)
import Url


type TaskModel model msg
    = TaskRunning (Cmd msg)
    | TaskComplete model


type TaskMsg model msg
    = TaskResult model
    | TaskWrapped msg


{-| Application that lets you run a task from the init function

This is a wrapper for Browser.application that expects a
`(Task Never Model, Cmd Msg)` instead of a `(Model, Cmd Msg)`
tuple from the init function.

An example of how this can be useful:

    init : () -> Url.Url -> Nav.Key -> ( Task Never Model, Cmd Msg )
    init flags url navKey =
        let
            task =
                Task.map2 toModel Time.now Time.here

            toModel time zone =
                { navKey = navKey
                , time = time
                , zone = zone
                }
        in
        ( task, Cmd.none )

-}
applicationWithTask :
    { init : flags -> Url.Url -> Navigation.Key -> ( Task Never model, Cmd msg )
    , loadingView : Browser.Document msg
    , view : model -> Browser.Document msg
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , onUrlRequest : Browser.UrlRequest -> msg
    , onUrlChange : Url.Url -> msg
    }
    -> Program flags (TaskModel model msg) (TaskMsg model msg)
applicationWithTask wrapped =
    let
        init flags url key =
            let
                ( task, wrappedCmd ) =
                    wrapped.init flags url key

                cmd =
                    Task.perform TaskResult task
            in
            ( TaskRunning wrappedCmd, cmd )

        view model =
            case model of
                TaskRunning _ ->
                    { title = wrapped.loadingView.title
                    , body = List.map (Html.map TaskWrapped) wrapped.loadingView.body
                    }

                TaskComplete wrappedModel ->
                    let
                        wrappedDoc =
                            wrapped.view wrappedModel
                    in
                    { title = wrappedDoc.title
                    , body = List.map (Html.map TaskWrapped) wrappedDoc.body
                    }

        update msg model =
            case model of
                TaskRunning initialCmd ->
                    case msg of
                        TaskResult result ->
                            ( TaskComplete result, Cmd.map TaskWrapped initialCmd )

                        TaskWrapped _ ->
                            ( model, Cmd.none )

                TaskComplete wrappedModel ->
                    case msg of
                        TaskResult _ ->
                            ( model, Cmd.none )

                        TaskWrapped wrappedMsg ->
                            let
                                ( newModel, cmd ) =
                                    wrapped.update wrappedMsg wrappedModel
                            in
                            ( TaskComplete newModel, Cmd.map TaskWrapped cmd )

        subscriptions model =
            case model of
                TaskRunning _ ->
                    Sub.none

                TaskComplete wrappedModel ->
                    Sub.map TaskWrapped (wrapped.subscriptions wrappedModel)

        onUrlRequest req =
            TaskWrapped (wrapped.onUrlRequest req)

        onUrlChange url =
            TaskWrapped (wrapped.onUrlChange url)
    in
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = onUrlRequest
        , onUrlChange = onUrlChange
        }


type DecoderModel model
    = DecodeFailure JD.Error
    | DecodeSuccess model


applicationWithDecoder :
    { init : flags -> Url.Url -> Navigation.Key -> ( model, Cmd msg )
    , view : model -> Browser.Document msg
    , errorView : JD.Error -> Browser.Document msg
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , onUrlRequest : Browser.UrlRequest -> msg
    , onUrlChange : Url.Url -> msg
    }
    -> JD.Decoder flags
    -> Program JD.Value (DecoderModel model) msg
applicationWithDecoder wrapped flagsDecoder =
    let
        init flags url key =
            case JD.decodeValue flagsDecoder flags of
                Ok decodedFlags ->
                    wrapped.init decodedFlags url key
                        |> Tuple.mapFirst DecodeSuccess

                Err err ->
                    ( DecodeFailure err, Cmd.none )

        view model =
            case model of
                DecodeFailure err ->
                    wrapped.errorView err

                DecodeSuccess wrappedModel ->
                    wrapped.view wrappedModel

        update msg model =
            case model of
                DecodeFailure err ->
                    ( model, Cmd.none )

                DecodeSuccess wrappedModel ->
                    wrapped.update msg wrappedModel
                        |> Tuple.mapFirst DecodeSuccess

        subscriptions model =
            case model of
                DecodeFailure err ->
                    Sub.none

                DecodeSuccess wrappedModel ->
                    wrapped.subscriptions wrappedModel
    in
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = wrapped.onUrlRequest
        , onUrlChange = wrapped.onUrlChange
        }
