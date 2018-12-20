module Vevapp.RemoteIp
    ( RemoteIp(..)
    , RealIpHeader(..)
    , Source(..)
    , formatIp
    , getRemoteIp
    ) where

import qualified Data.IP as IP
import qualified Data.Text as T
import qualified GHC.Generics as GHC
import qualified Network.Socket as Socket
import qualified Web.HttpApiData as HttpApiData


newtype RemoteIp = RemoteIp T.Text
    deriving (Show, GHC.Generic)


newtype RealIpHeader = RealIpHeader T.Text
    deriving (Show, GHC.Generic)


instance HttpApiData.FromHttpApiData RealIpHeader where
    parseUrlPiece text = Right $ RealIpHeader text



formatIp :: Socket.SockAddr -> T.Text
formatIp sockAddr =
    case sockAddr of
        Socket.SockAddrInet _ hostAddr ->
            T.pack $ show $ IP.fromHostAddress hostAddr

        Socket.SockAddrInet6 _ _ hostAddr6 _ ->
            T.pack $ show $ IP.fromHostAddress6 hostAddr6

        _ ->
            "N/A"


data Source
    = Socket
    | RealIp
    deriving (Show, Eq)


instance Read Source where
    readsPrec _ value =
        case value of
            "socket" ->
                [(Socket, "")]

            "real_ip" ->
                [(RealIp, "")]

            _ ->
                []


getRemoteIp :: Source -> Socket.SockAddr -> Maybe RealIpHeader -> RemoteIp
getRemoteIp source sockAddr maybeRealIp =
    case source of
        Socket ->
            RemoteIp $ formatIp sockAddr

        RealIp ->
            case maybeRealIp of
                Nothing ->
                    RemoteIp "N/A"

                Just (RealIpHeader "") ->
                    RemoteIp "N/A"

                Just (RealIpHeader realIp) ->
                    RemoteIp realIp
