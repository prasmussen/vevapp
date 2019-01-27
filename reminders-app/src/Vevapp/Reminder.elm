module Vevapp.Reminder exposing
    ( ListOptions
    , Reminder
    , apiResponseDecoder
    , decoder
    , encodeListOptions
    )

import Iso8601
import Json.Decode as JD
import Json.Encode as JE
import Time


type alias Reminder =
    { summary : String
    , htmlLink : String
    , startDate : Time.Posix
    }


apiResponseDecoder : JD.Decoder (List Reminder)
apiResponseDecoder =
    JD.at [ "items" ] (JD.list decoder)


decoder : JD.Decoder Reminder
decoder =
    JD.map3 Reminder
        (JD.field "summary" JD.string)
        (JD.field "htmlLink" JD.string)
        (JD.at [ "start", "dateTime" ] Iso8601.decoder)


type alias ListOptions =
    { calendarId : String
    , maxResults : Int
    , timeMin : String
    , timeMax : String
    , orderBy : String
    , singleEvents : Bool
    , privateExtendedProperty : String
    }


encodeListOptions : ListOptions -> JE.Value
encodeListOptions options =
    JE.object
        [ ( "calendarId", JE.string options.calendarId )
        , ( "maxResults", JE.int options.maxResults )
        , ( "timeMin", JE.string options.timeMin )
        , ( "timeMax", JE.string options.timeMax )
        , ( "orderBy", JE.string options.orderBy )
        , ( "singleEvents", JE.bool options.singleEvents )
        , ( "privateExtendedProperty", JE.string options.privateExtendedProperty )
        ]
