{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExtendedDefaultRules #-}


module Vevapp.Api.Root
    ( root
    ) where


import qualified Network.Socket as Socket
import Servant
import qualified Vevapp.Config as Config
import qualified Vevapp.IpInfo as IpInfo
import qualified Vevapp.RemoteIp as RemoteIp


root :: Config.Config -> Socket.SockAddr -> Maybe RemoteIp.RealIpHeader -> Handler IpInfo.IpInfo
root config socket ipHeader =
    let
        ipSource =
            Config.remoteIpSource config

        remoteIp =
            RemoteIp.getRemoteIp ipSource socket ipHeader
    in
    return $ IpInfo.fromRemoteIp remoteIp
