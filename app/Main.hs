{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Control.Monad.State
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified ENV
import Infrastructures.ContaierIO (ContainerIO (..), ContainerIOState (..))
import Pre
import Usecase.AIChatService (runAIChatService)
import VoiceBox (getSpeakerStyleId)

main :: IO ()
main = do
  -- Load .env file
  evs <- ENV.loadEnvVars
  let url = ENV.ollamaUrl evs
      model = ENV.ollamaModel evs
      vbUrl = ENV.voiceBoxUrl evs
      voiceBoxSpeaker = ENV.voiceBoxSpeaker evs
      voiceBoxStyleName = ENV.voiceBoxStyleName evs

  T.putStrLn $ "Using Ollama URL: " <> url
  T.putStrLn $ "Using Ollama Model: " <> model

  mstyleId <- do
    case vbUrl of
      "" -> do
        T.putStrLn "VoiceBox URL not set"
        return Nothing
      _ -> do
        -- Display VoiceBox information
        T.putStrLn $ "Using VoiceBox URL: " <> vbUrl
        T.putStrLn $ "Using VoiceBox Speaker: " <> voiceBoxSpeaker
        T.putStrLn $ "Using VoiceBox Style: " <> voiceBoxStyleName
        edid <- getSpeakerStyleId voiceBoxSpeaker voiceBoxStyleName vbUrl Nothing
        case edid of
          Left err -> do
            T.putStrLn $ "Failed to get speaker style ID: " <> err
            return Nothing
          Right styleId -> do
            T.putStrLn $ "Using VoiceBox Style ID: " <> T.pack (show styleId)
            return $ Just styleId

  let config = ContainerIOState url model vbUrl mstyleId []
  void $ runStateT (runContainerIO runAIChatService) config