{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Usecase.TTS
  ( ITextToSpeech (..),
  )
where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import Pre

class (MonadIO m) => ITextToSpeech m where
  -- | Play a message using the VoiceBox service
  textToSpeech :: Text -> m (Either Text BL.ByteString)
