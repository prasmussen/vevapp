{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExtendedDefaultRules #-}


module Vevapp.Api.Root
    ( root
    ) where

import qualified Control.Monad.IO.Class as IO
import qualified Data.Time.Clock as Clock
import Servant
import qualified Vevapp.WeekInfo as WeekInfo


root :: Handler WeekInfo.WeekInfo
root = do
    time <- IO.liftIO Clock.getCurrentTime
    return $ WeekInfo.WeekInfo time
