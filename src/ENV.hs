{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ENV
  ( AITalkEnvVars (..),
    loadEnvVars,
  )
where

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import Pre
import System.Environment (lookupEnv)

data AITalkEnvVars = AITalkEnvVars
  { ollamaUrl :: Text,
    ollamaModel :: Text,
    voiceBoxUrl :: Text,
    voiceBoxSpeaker :: Text,
    voiceBoxStyleName :: Text
  }

-- Get Ollama URL from environment variable (default "")
getOllamaURL :: IO Text
getOllamaURL = do
  mUrl <- lookupEnv "OLLAMA_URL"
  return $ maybe "" T.pack mUrl

-- Get Ollama model from environment variable (default "")
getOllamaModel :: IO Text
getOllamaModel = do
  mModel <- lookupEnv "OLLAMA_MODEL"
  return $ maybe "" T.pack mModel

-- Get VoiceBox URL from environment variable (default "")
getVoiceBoxURL :: IO Text
getVoiceBoxURL = do
  mUrl <- lookupEnv "VOICE_BOX_URL"
  return $ maybe "" T.pack mUrl

-- Get VoiceBox speaker from environment variable (default "")
getVoiceBoxSpeaker :: IO Text
getVoiceBoxSpeaker = do
  mSpeaker <- lookupEnv "VOICE_BOX_SPEAKER"
  return $ maybe "" T.pack mSpeaker

-- Get VoiceBox style name from environment variable (default "")
getVoiceBoxStyleName :: IO Text
getVoiceBoxStyleName = do
  mStyleName <- lookupEnv "VOICE_BOX_STYLE_NAME"
  return $ maybe "" T.pack mStyleName

-- | load environment variables from .env file
loadEnvVars :: IO AITalkEnvVars
loadEnvVars = do
  void $ loadFile defaultConfig
  AITalkEnvVars <$> getOllamaURL <*> getOllamaModel <*> getVoiceBoxURL <*> getVoiceBoxSpeaker <*> getVoiceBoxStyleName