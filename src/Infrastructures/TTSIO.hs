{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Infrastructures.TTSIO
  ( TTSIOSate (..),
    TTSIO (..),
    getTTSIOState,
  )
where

import Control.Monad.State
import Data.Text (Text)
import Pre
import Usecase.TTS (ITextToSpeech (..))
import qualified VoiceBox as VB

data TTSIOSate = TTSIOSate
  { voiceBoxStyleId :: Maybe Int,
    voiceBoxUrl :: Text
  }

newtype TTSIO a = TTSIO
  { runTTSIO :: StateT TTSIOSate IO a
  }

instance Functor TTSIO where
  fmap f mx = TTSIO $ do
    x <- runTTSIO mx
    return (f x)

instance Applicative TTSIO where
  pure x = TTSIO $ return x
  mf <*> mx = TTSIO $ do
    f <- runTTSIO mf
    x <- runTTSIO mx
    return $ f x

instance Monad TTSIO where
  return = pure
  mx >>= f = TTSIO $ do
    x <- runTTSIO mx
    runTTSIO $ f x

instance MonadIO TTSIO where
  liftIO = TTSIO . liftIO

getTTSIOState :: TTSIO TTSIOSate
getTTSIOState = TTSIO get

instance ITextToSpeech TTSIO where
  textToSpeech text = do
    s <- getTTSIOState
    case voiceBoxStyleId s of
      Just styleId -> liftIO $ VB.textToSpeech (voiceBoxUrl s) text styleId Nothing
      Nothing -> return $ Left "No style id"