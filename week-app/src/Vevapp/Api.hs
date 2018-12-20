{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}


module Vevapp.Api
    ( app
    ) where


import qualified Servant
import qualified Vevapp.Api.Routes as Routes
import qualified Vevapp.Config as Config


type AppAPI
    = Routes.Api


appApi :: Servant.Proxy AppAPI
appApi =
    Servant.Proxy


appServer :: Config.Config -> Servant.Server AppAPI
appServer config =
    Routes.server config


app :: Config.Config -> Servant.Application
app config =
    Servant.serve appApi (appServer config)
