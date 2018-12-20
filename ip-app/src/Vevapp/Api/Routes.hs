{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Vevapp.Api.Routes
    ( Api
    , server
    ) where

import Servant
import Servant.HTML.Lucid
import qualified Vevapp.Api.Root as Root
import qualified Vevapp.Config as Config
import qualified Vevapp.IpInfo as IpInfo
import qualified Vevapp.RemoteIp as RemoteIp



type Api
      =  RootRoute
    :<|> StaticRoute


server :: Config.Config -> Server Api
server config
      =  Root.root config
    :<|> serveDirectoryWebApp (Config.staticPathStr config)


type RootRoute =
    RemoteHost
    :> Header "X-Real-Ip" RemoteIp.RealIpHeader
    :> Get '[PlainText, JSON, HTML] IpInfo.IpInfo


type StaticRoute =
    "static"
    :> Raw
