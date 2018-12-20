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
import qualified Vevapp.WeekInfo as WeekInfo



type Api
      =  RootRoute
    :<|> StaticRoute


server :: Config.Config -> Server Api
server config
      =  Root.root
    :<|> serveDirectoryWebApp (Config.staticPathStr config)


type RootRoute =
    Get '[PlainText, JSON, HTML] WeekInfo.WeekInfo


type StaticRoute =
    "static"
    :> Raw
