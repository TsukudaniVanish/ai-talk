{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
module Main (main) where

import Pre
import Data.Text (Text)
import System.IO (hFlush, stdout)
import qualified Data.Text.IO as T
import Chat(chat, ChatMessage(..), ChatResponse(..), ChatRequest(..))
import System.Environment (lookupEnv)
import qualified Data.Text as T
import Configuration.Dotenv (loadFile, defaultConfig)
import Control.Monad (void)

main :: IO ()
main = do
  -- .envファイルを読み込む
  void $ loadFile defaultConfig
  url <- getOllamaURL
  model <- getOllamaModel
  T.putStrLn $ "Using Ollama URL: " <> url
  T.putStrLn $ "Using Ollama Model: " <> model
  T.putStrLn "Ollama chatbot started"
  loop [] url model

-- 環境変数からOllama URLを取得（デフォルト ""）
getOllamaURL :: IO Text
getOllamaURL = do
  mUrl <- lookupEnv "OLLAMA_URL"
  return $ maybe "" T.pack mUrl

-- 環境変数からOllamaモデルを取得（デフォルト ""）
getOllamaModel :: IO Text
getOllamaModel = do
  mModel <- lookupEnv "OLLAMA_MODEL"
  return $ maybe "" T.pack mModel


-- 通常のループ
loop :: [ChatMessage] -> Text -> Text -> IO ()
loop history url model = do
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
        Left e -> T.putStrLn e >> loop history url model
        Right r -> do
          let botMessage = resMessage r
          T.putStrLn $ "assistant: " <> content botMessage
          loop (messages ++ [botMessage]) url model



