{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExtendedDefaultRules #-}


module Vevapp.Api.Root
    ( root
    ) where


import qualified Control.Monad.IO.Class as IO
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Random as Random
import qualified Servant
import qualified Vevapp.Config as Config
import qualified Vevapp.FourWords as FourWords
import qualified Vevapp.WordList as WordList


root :: Config.Config -> Servant.Handler FourWords.FourWords
root config =
    let
        wordList =
            Config.wordList config

        randomElement =
            Random.sample . Random.randomElement

        randomWord list =
            IO.liftIO $ randomElement (NonEmpty.toList list)
    in do
    adjective <- randomWord (WordList.adjectives wordList)
    verb <- randomWord (WordList.verbs wordList)
    noun1 <- randomWord (WordList.nouns wordList)
    noun2 <- randomWord (WordList.nouns wordList)
    return $ FourWords.FourWords adjective noun1 verb noun2
