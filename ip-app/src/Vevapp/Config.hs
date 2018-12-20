module Vevapp.Config
    ( Config(..)
    , ListenPort(..)
    , ListenHost(..)
    , StaticPath(..)
    , staticPathStr
    )
    where


import qualified Data.String as String
import qualified Data.Text as T
import qualified Network.Wai.Handler.Warp as Warp
import qualified Text.Read as Read
import qualified Vevapp.RemoteIp as RemoteIp


data Config = Config
    { staticPath :: StaticPath
    , remoteIpSource :: RemoteIp.Source
    }
    deriving (Show)



staticPathStr :: Config -> String
staticPathStr Config { staticPath = StaticPath path } =
    T.unpack path


newtype ListenPort = ListenPort Int
    deriving (Show)

instance Read ListenPort where
    readsPrec _ = readMaybe ListenPort



newtype ListenHost = ListenHost Warp.HostPreference
    deriving (Show)

instance Read ListenHost where
    readsPrec _ = readHostPreference ListenHost



newtype StaticPath = StaticPath T.Text
    deriving (Show)

instance Read StaticPath where
    readsPrec _ = readText StaticPath



readMaybe :: Read b => (b -> a) -> String -> [(a, String)]
readMaybe constructor str =
    case Read.readMaybe str :: Read b => Maybe b of
        Just value ->
            [(constructor value, "")]

        Nothing ->
            []


readText :: (T.Text -> a) -> String -> [(a, String)]
readText constructor str =
    [(constructor $ T.pack str, "")]


readHostPreference :: (Warp.HostPreference -> a) -> String -> [(a, String)]
readHostPreference constructor str =
    [(constructor $ String.fromString str, "")]
