module Vevapp.WordList
    ( WordList(..)
    )
    where


import qualified Data.Aeson as Aeson
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as T
import qualified GHC.Generics as GHC
import qualified Vevapp.Word as Word


data WordList = WordList
    { adjectives :: NonEmpty.NonEmpty Word.Adjective
    , nouns :: NonEmpty.NonEmpty Word.Noun
    , verbs :: NonEmpty.NonEmpty Word.Verb
    }
    deriving (Show, GHC.Generic)


instance Aeson.FromJSON WordList
