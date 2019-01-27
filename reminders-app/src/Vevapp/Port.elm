port module Vevapp.Port exposing
    ( MessageFromJavascript(..)
    , MessageToJavascript(..)
    , receive
    , send
    )

import Json.Decode as JD
import Json.Encode as JE
import Vevapp.Reminder as Reminder exposing (Reminder)


type MessageToJavascript
    = ListReminders Reminder.ListOptions


type MessageFromJavascript
    = ListRemindersSuccess (List Reminder)
    | ListRemindersFailure String


type alias JavascriptMsg =
    { tag : String
    , data : JE.Value
    }


jsMsgDecoder : JD.Decoder JavascriptMsg
jsMsgDecoder =
    JD.map2 JavascriptMsg
        (JD.field "tag" JD.string)
        (JD.field "data" JD.value)


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
        decodePayload : JE.Value -> msg
        decodePayload value =
            case JD.decodeValue jsMsgDecoder value of
                Err err ->
                    onError <|
                        String.concat
                            [ "Unexpected message from javascript: "
                            , JD.errorToString err
                            ]

                Ok jsMsg ->
                    case jsMsg.tag of
                        "ListRemindersSuccess" ->
                            case JD.decodeValue Reminder.apiResponseDecoder jsMsg.data of
                                Ok reminders ->
                                    tagger (ListRemindersSuccess reminders)

                                Err err ->
                                    onError (JD.errorToString err)

                        "ListRemindersFailure" ->
                            case JD.decodeValue JD.string jsMsg.data of
                                Ok err ->
                                    tagger (ListRemindersFailure err)

                                Err err ->
                                    onError (JD.errorToString err)

                        _ ->
                            onError <|
                                String.concat
                                    [ "Unexpected message from javascript: "
                                    , jsMsg.tag
                                    ]
    in
    fromJavascript decodePayload


port toJavascript : JavascriptMsg -> Cmd msg


port fromJavascript : (JE.Value -> msg) -> Sub msg
