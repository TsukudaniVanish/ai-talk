{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Chat (ChatMessage (..), ChatRequest (..), ChatResponse (..), chat)
import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Monad (unless, void)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Pre
import System.Environment (lookupEnv)
import System.IO (hFlush, stdout)
import VoiceBox (getSpeakerStyleId, textToSpeech)
import WAV (playWavOpenAL)

main :: IO ()
main = do
  -- Load .env file
  void $ loadFile defaultConfig
  url <- getOllamaURL
  model <- getOllamaModel
  vbUrl <- getVoiceBoxURL
  voiceBoxSpeaker <- getVoiceBoxSpeaker
  voiceBoxStyleName <- getVoiceBoxStyleName

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
        edid <- getSpeakerStyleId vbUrl voiceBoxSpeaker voiceBoxStyleName Nothing
        case edid of
          Left err -> do
            T.putStrLn $ "Failed to get speaker style ID: " <> err
            return Nothing
          Right styleId -> do
            T.putStrLn $ "Using VoiceBox Style ID: " <> T.pack (show styleId)
            return $ Just styleId

  let config = AITalkConfig url model vbUrl mstyleId
  loop [] config

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

-- Normal conversation loop
loop :: [ChatMessage] -> AITalkConfig -> IO ()
loop history config = do
  input <- getUserInput
  let command = parseUserInput input
  case command of
    QuitCommand -> T.putStrLn "Goodbye!"
    MessageCommand msg -> do
      newHistory <- processMessage history config msg
      loop newHistory config

data CUICommand
  = QuitCommand
  | MessageCommand Text

data AITalkConfig = AITalkConfig
  { ollamaUrl :: Text,
    ollamaModel :: Text,
    voiceBoxUrl :: Text,
    voiceBoxStyleId :: Maybe Int
  }

-- | Get user input from the console
getUserInput :: IO Text
getUserInput = do
  T.putStr "You: "
  hFlush stdout
  T.getLine

-- | Parse user input to determine the command type
parseUserInput :: Text -> CUICommand
parseUserInput input
  | input == ":q" || input == ":quit" = QuitCommand
  | otherwise = MessageCommand input

-- | Process user message by sending to AI and handling the response
processMessage :: [ChatMessage] -> AITalkConfig -> Text -> IO [ChatMessage]
processMessage history config input = do
  let userMessage = ChatMessage "user" input
      messages = history ++ [userMessage]
      req = ChatRequest (ollamaModel config) messages False
  o <- chat req (ollamaUrl config)
  case o of
    Left e -> do
      T.putStrLn e
      return history
    Right r -> do
      let botMessage = resMessage r
          botContent = content botMessage
      T.putStrLn $ "assistant: " <> botContent

      -- If VoiceBox URL is set, perform speech synthesis and playback
      case voiceBoxStyleId config of
        Just styleId -> do
          audioResult <- textToSpeech (voiceBoxUrl config) botContent styleId Nothing
          case audioResult of
            Left err -> T.putStrLn $ "Failed to generate audio: " <> err
            Right wavData -> do
              T.putStrLn "Playing audio response..."
              playResult <- playWavOpenAL wavData
              unless playResult $ T.putStrLn "Failed to play audio"
        Nothing -> return ()
      return (messages ++ [botMessage])