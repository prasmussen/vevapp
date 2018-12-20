module Vevapp.Config
    ( Config(..)
    , ListenPort(..)
    , StaticPath(..)
    , staticPathStr
    )
    where


import qualified Data.Text as T
import qualified Text.Read as Read


data Config = Config
    { staticPath :: StaticPath
    }



staticPathStr :: Config -> String
staticPathStr Config { staticPath = StaticPath path } =
    T.unpack path


newtype ListenPort = ListenPort Int
    deriving (Show)

instance Read ListenPort where
    readsPrec _ = readMaybe ListenPort


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
