{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import AIChatService (AIChatService (..))
import CUI (CUICommand (..), getUserInput, parseUserInput)
import Control.Monad.State
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified ENV
import Pre
import Usecase.AIChatService
  ( AITalkConfig (..),
    IAIChartService (..),
    processMessage,
  )
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

  let config = AITalkConfig url model vbUrl mstyleId
  void $ runStateT (runAIChatService loop) ([], config)

-- Normal conversation loop
loop :: (IAIChartService m) => m ()
loop = do
  input <- liftIO getUserInput
  let command = parseUserInput input
  case command of
    QuitCommand -> liftIO $ T.putStrLn "Goodbye!"
    MessageCommand msg -> do
      processMessage msg
      loop
