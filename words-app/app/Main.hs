module Main where



import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.Coerce as Coerce
import Data.Function ((&))
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Network.Wai.Handler.Warp as Warp
import qualified Safe
import qualified System.Environment as Env
import qualified Vevapp.Api as Api
import qualified Vevapp.Config as Config
import qualified Vevapp.WordList as WordList


main :: IO ()
main = do
    listenPort <- lookupSetting "LISTEN_PORT" (Config.ListenPort 8081)
    listenHost <- lookupSetting "LISTEN_HOST" (Config.ListenHost "*4")
    wordList <- readWordList
    config <- mkConfig wordList
    print (listenHost, listenPort)
    Warp.runSettings (warpSettings listenPort listenHost) (Api.app config)


readWordList :: IO WordList.WordList
readWordList = do
    wordListPath <- lookupSetting "WORD_LIST_PATH" (Config.WordListPath "./wordlist.json")
    jsonData <- BS.readFile (T.unpack $ Coerce.coerce wordListPath)
    case Aeson.eitherDecodeStrict' jsonData of
        Left err ->
            error $ mconcat
                [ "Failed to parse "
                , T.unpack (Coerce.coerce wordListPath)
                , " into WordList, error: "
                , err
                ]

        Right wordList ->
            return wordList


mkConfig :: WordList.WordList -> IO Config.Config
mkConfig wordList = do
    staticPath <- lookupSetting "STATIC_PATH" (Config.StaticPath "./static")
    return $ Config.Config
        { Config.staticPath = staticPath
        , Config.wordList = wordList
        }


warpSettings :: Config.ListenPort -> Config.ListenHost -> Warp.Settings
warpSettings (Config.ListenPort port) (Config.ListenHost host) =
    Warp.defaultSettings
    & Warp.setHost host
    & Warp.setPort port



lookupSetting :: Read a => String -> a -> IO a
lookupSetting envKey def = do
    maybeValue <- Env.lookupEnv envKey
    case maybeValue of
        Just str ->
            return $ readSetting envKey str

        Nothing ->
            return def


readSetting :: Read a => String -> String -> a
readSetting envKey str =
    case Safe.readMay str of
        Just value ->
            value

        Nothing ->
            error $ mconcat
                [ "«"
                , str
                , "» is not a valid value for the environment variable "
                , envKey
                ]
