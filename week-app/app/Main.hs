module Main where

import qualified Data.Coerce as Coerce
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Network.Wai.Handler.Warp as Warp
import qualified Safe
import qualified System.Environment as Env
import qualified Vevapp.Api as Api
import qualified Vevapp.Config as Config


main :: IO ()
main = do
    listenPort <- lookupSetting "LISTEN_PORT" (Config.ListenPort 8081)
    staticPath <- lookupSetting "STATIC_PATH" (Config.StaticPath "./static")
    let config = Config.Config { Config.staticPath = staticPath }
    putStrLn $ "Listening on port: " <> (show listenPort)
    Warp.run (Coerce.coerce listenPort) (Api.app config)



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
