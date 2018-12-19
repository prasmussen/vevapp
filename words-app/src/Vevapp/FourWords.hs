module Vevapp.FourWords
    ( FourWords(..)
    )
    where


import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Coerce as Coerce
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as Encoding
import Lucid
import qualified Servant
import qualified Vevapp.Word as Word



data FourWords = FourWords Word.Adjective Word.Noun Word.Verb Word.Noun
    deriving (Show)


instance ToHtml FourWords where
    toHtml fourWords =
        let
            words =
                toHtml $ toText fourWords
        in
        doctypehtml_ $ do
            head_ $ do
                meta_ [ charset_ "UTF-8" ]
                meta_ [ name_ "viewport", content_ "width=device-width, initial-scale=1" ]
                title_ (words)
                link_ [ rel_ "stylesheet", type_ "text/css", href_ "/static/styles.css" ]
            body_ $ do
                h1_ words

    toHtmlRaw =
        toHtml


instance Aeson.ToJSON FourWords where
    toJSON (FourWords adjective noun1 verb noun2) =
        Aeson.object
            [ "adjective" .= adjective
            , "noun1" .= noun1
            , "verb" .= verb
            , "noun2" .= noun2
            ]


instance Servant.MimeRender Servant.PlainText FourWords where
    mimeRender _ fourWords =
        BSL.fromStrict $ Encoding.encodeUtf8 $ toText fourWords


toText :: FourWords -> T.Text
toText (FourWords adjective noun1 verb noun2) =
    mconcat
        [ Coerce.coerce adjective
        , " "
        , Coerce.coerce noun1
        , " "
        , Coerce.coerce verb
        , " "
        , Coerce.coerce noun2
        ]
