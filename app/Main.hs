{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Chat (ChatMessage (..), ChatRequest (..), ChatResponse (..), chat)
import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Monad (unless, void, when)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Pre
  ( Bool (False),
    Either (Left, Right),
    Eq ((/=), (==)),
    IO,
    Maybe (Just, Nothing),
    Monad (return, (>>)),
    Semigroup ((<>)),
    Show (show),
    maybe,
    ($),
    (++),
    (||),
  )
import System.Environment (lookupEnv)
import System.IO (hFlush, stdout)
import VoiceBox (findSpeakerStyle, getSpeakers, textToSpeech)
import WAV (playWavOpenAL)

main :: IO ()
main = do
  -- Load .env file
  void $ loadFile defaultConfig
  url <- getOllamaURL
  model <- getOllamaModel
  voiceBoxUrl <- getVoiceBoxURL
  voiceBoxSpeaker <- getVoiceBoxSpeaker
  voiceBoxStyleName <- getVoiceBoxStyleName

  T.putStrLn $ "Using Ollama URL: " <> url
  T.putStrLn $ "Using Ollama Model: " <> model

  -- Display VoiceBox information
  when (voiceBoxUrl /= "") $ do
    T.putStrLn $ "Using VoiceBox URL: " <> voiceBoxUrl
    T.putStrLn $ "Using VoiceBox Speaker: " <> voiceBoxSpeaker
    T.putStrLn $ "Using VoiceBox Style: " <> voiceBoxStyleName

    -- Retrieve speaker information and confirm style ID
    speakersResult <- getSpeakers voiceBoxUrl Nothing
    case speakersResult of
      Left err -> T.putStrLn $ "Failed to get speakers: " <> err
      Right speakers -> do
        case findSpeakerStyle voiceBoxSpeaker voiceBoxStyleName speakers of
          Nothing -> T.putStrLn "Specified speaker or style not found"
          Just styleId -> T.putStrLn $ "Found style ID: " <> T.pack (show styleId)

  T.putStrLn "Ollama chatbot started"
  loop [] url model voiceBoxUrl voiceBoxSpeaker voiceBoxStyleName

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
loop :: [ChatMessage] -> Text -> Text -> Text -> Text -> Text -> IO ()
loop history url model voiceBoxUrl voiceBoxSpeaker voiceBoxStyleName = do
  T.putStr "You: "
  hFlush stdout
  l <- T.getLine
  if l == ":q" || l == ":quit"
    then T.putStrLn "Goodbye!"
    else do
      let userMessage = ChatMessage "user" l
          messages = history ++ [userMessage]
          req = ChatRequest model messages False
      o <- chat req url
      case o of
        Left e -> T.putStrLn e >> loop history url model voiceBoxUrl voiceBoxSpeaker voiceBoxStyleName
        Right r -> do
          let botMessage = resMessage r
              botContent = content botMessage
          T.putStrLn $ "assistant: " <> botContent

          -- If VoiceBox URL is set, perform speech synthesis and playback
          when (voiceBoxUrl /= "") $ do
            speakersResult <- getSpeakers voiceBoxUrl Nothing
            case speakersResult of
              Left err -> T.putStrLn $ "Failed to get speakers: " <> err
              Right speakers -> do
                case findSpeakerStyle voiceBoxSpeaker voiceBoxStyleName speakers of
                  Nothing -> T.putStrLn "Specified speaker or style not found"
                  Just styleId -> do
                    audioResult <- textToSpeech voiceBoxUrl botContent styleId Nothing
                    case audioResult of
                      Left err -> T.putStrLn $ "Failed to generate audio: " <> err
                      Right wavData -> do
                        T.putStrLn "Playing audio response..."
                        playResult <- playWavOpenAL wavData
                        unless playResult $ T.putStrLn "Failed to play audio"

          loop (messages ++ [botMessage]) url model voiceBoxUrl voiceBoxSpeaker voiceBoxStyleName
