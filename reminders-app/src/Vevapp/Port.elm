port module Vevapp.Port exposing
    ( MessageFromJavascript(..)
    , MessageToJavascript(..)
    , receive
    , send
    )

import Json.Decode as Decode
import Json.Encode as Encode
import Vevapp.Reminder as Reminder exposing (Reminder)


type MessageToJavascript
    = ListReminders Reminder.ListOptions


type MessageFromJavascript
    = ListRemindersSuccess (List Reminder)
    | ListRemindersFailure String


type alias JavascriptPayload =
    { tag : String
    , data : Encode.Value
    }


send : MessageToJavascript -> Cmd msg
send info =
    case info of
        ListReminders options ->
            toJavascript
                { tag = "ListReminders"
                , data = Reminder.encodeListOptions options
                }


receive : (MessageFromJavascript -> msg) -> (String -> msg) -> Sub msg
receive tagger onError =
    let
        decodePayload : JavascriptPayload -> msg
        decodePayload payload =
            case payload.tag of
                "ListRemindersFailure" ->
                    case Decode.decodeValue Decode.string payload.data of
                        Ok err ->
                            tagger (ListRemindersFailure err)

                        Err err ->
                            onError (Decode.errorToString err)

                "ListRemindersSuccess" ->
                    case Decode.decodeValue (Decode.list Reminder.decoder) payload.data of
                        Ok reminders ->
                            tagger (ListRemindersSuccess reminders)

                        Err err ->
                            onError (Decode.errorToString err)

                _ ->
                    onError <|
                        String.concat
                            [ "Unexpected message from javascript: "
                            , payload.tag
                            ]
    in
    fromJavascript decodePayload


port toJavascript : JavascriptPayload -> Cmd msg


port fromJavascript : (JavascriptPayload -> msg) -> Sub msg
