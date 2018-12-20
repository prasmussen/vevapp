module Vevapp.WeekInfo
    ( WeekInfo(..)
    , getWeek
    ) where


import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Monoid ((<>))
import qualified Data.Time.Calendar as Calendar
import qualified Data.Time.Calendar.WeekDate as WeekDate
import qualified Data.Time.Clock as Clock
import Lucid
import qualified Servant



newtype WeekInfo = WeekInfo Clock.UTCTime
    deriving (Show)


instance ToHtml WeekInfo where
    toHtml weekInfo =
        let
            week =
                toHtml $ show $ getWeek weekInfo
        in
        doctypehtml_ $ do
            head_ $ do
                meta_ [ charset_ "UTF-8" ]
                meta_ [ name_ "viewport", content_ "width=device-width, initial-scale=1, shrink-to-fit=no" ]
                title_ ("Week: " <> week)
                link_ [ rel_ "stylesheet", type_ "text/css", href_ "/static/styles.css" ]
            body_ $ do
                h1_ week

    toHtmlRaw =
        toHtml


instance Aeson.ToJSON WeekInfo where
    toJSON weekInfo =
        Aeson.object
            [ "week" .= getWeek weekInfo
            ]


instance Servant.MimeRender Servant.PlainText WeekInfo where
    mimeRender _ weekInfo =
        BSL8.pack $ show $ getWeek weekInfo


getWeek :: WeekInfo -> Int
getWeek (WeekInfo time) =
    weekOfYear
    where
        day =
            Clock.utctDay time

        (_, weekOfYear, _) =
            WeekDate.toWeekDate day
