module Vevapp.Word
    ( Verb(..)
    , Adjective(..)
    , Noun(..)
    )
    where


import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified GHC.Generics as GHC



newtype Adjective = Adjective T.Text
    deriving (Show, GHC.Generic)

instance Aeson.FromJSON Adjective
instance Aeson.ToJSON Adjective


newtype Noun = Noun T.Text
    deriving (Show, GHC.Generic)

instance Aeson.FromJSON Noun
instance Aeson.ToJSON Noun


newtype Verb = Verb T.Text
    deriving (Show, GHC.Generic)

instance Aeson.FromJSON Verb
instance Aeson.ToJSON Verb


