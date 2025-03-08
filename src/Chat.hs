{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Chat(chat, ChatMessage(..), ChatResponse(..), ChatRequest(..), chatWithText, ChatStreamCallback) where

import Pre
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.ByteString.Lazy.Char8 as L8
import GHC.Generics (Generic)
import Network.HTTP.Simple
import Data.Aeson (encode, object, withObject, (.=), (.:), decode, FromJSON(..), ToJSON(..))

data ChatMessage = ChatMessage
  { role :: Text
  , content :: Text
  } deriving (Show, Generic)

instance FromJSON ChatMessage
instance ToJSON ChatMessage

data ChatRequest = ChatRequest
  { reqModel :: Text
  , reqMessages :: [ChatMessage]
  , reqStream :: Bool
  } deriving (Show, Generic)

instance FromJSON ChatRequest where
    parseJSON = withObject "ChatRequest" $ \v -> ChatRequest
        <$> v .: "model"
        <*> v .: "messages"
        <*> v .: "stream"
instance ToJSON ChatRequest where
    toJSON (ChatRequest model messages stream) = object
        [ "model" .= model
        , "messages" .= messages
        , "stream" .= stream
        ]

data ChatResponse = ChatResponse
    { resModel :: Text
    , resCreatedAt :: Text
    , resMessage :: ChatMessage
    , resDone :: Bool
    , resTotalDuration :: Int
    , resLoadDuration :: Int
    , resPromptEvalCount :: Int
    , resPromptEvalDuration :: Int
    , resEvalCount :: Int
    , resEvalDuration :: Int
    } deriving (Show, Generic)

instance FromJSON ChatResponse where
    parseJSON = withObject "ChatResponse" $ \v -> ChatResponse
        <$> v .: "model"
        <*> v .: "created_at"
        <*> v .: "message"
        <*> v .: "done"
        <*> v .: "total_duration"
        <*> v .: "load_duration"
        <*> v .: "prompt_eval_count"
        <*> v .: "prompt_eval_duration"
        <*> v .: "eval_count"
        <*> v .: "eval_duration"
instance ToJSON ChatResponse where
    toJSON (ChatResponse model created_at message done total_duration load_duration prompt_eval_count prompt_eval_duration eval_count eval_duration) = object
        [ "model" .= model
        , "created_at" .= created_at
        , "message" .= message
        , "done" .= done
        , "total_duration" .= total_duration
        , "load_duration" .= load_duration
        , "prompt_eval_count" .= prompt_eval_count
        , "prompt_eval_duration" .= prompt_eval_duration
        , "eval_count" .= eval_count
        , "eval_duration" .= eval_duration
        ]

-- ストリーミングレスポンス用のデータ型
data ChatStreamResponse = ChatStreamResponse
  { strModel :: Text
  , strCreatedAt :: Text
  , strMessage :: Maybe ChatMessage
  , strDone :: Bool
  , strTotalDuration :: Maybe Int
  , strLoadDuration :: Maybe Int
  , strPromptEvalCount :: Maybe Int
  , strPromptEvalDuration :: Maybe Int
  , strEvalCount :: Maybe Int
  , strEvalDuration :: Maybe Int
  } deriving (Show, Generic)

instance FromJSON ChatStreamResponse where
    parseJSON = withObject "ChatStreamResponse" $ \v -> ChatStreamResponse
        <$> v .: "model"
        <*> v .: "created_at"
        <*> v .: "message"
        <*> v .: "done"
        <*> v .: "total_duration"
        <*> v .: "load_duration"
        <*> v .: "prompt_eval_count"
        <*> v .: "prompt_eval_duration"
        <*> v .: "eval_count"
        <*> v .: "eval_duration"
instance ToJSON ChatStreamResponse where
    toJSON (ChatStreamResponse model created_at message done total_duration load_duration prompt_eval_count prompt_eval_duration eval_count eval_duration) = object
        [ "model" .= model
        , "created_at" .= created_at
        , "message" .= message
        , "done" .= done
        , "total_duration" .= total_duration
        , "load_duration" .= load_duration
        , "prompt_eval_count" .= prompt_eval_count
        , "prompt_eval_duration" .= prompt_eval_duration
        , "eval_count" .= eval_count
        , "eval_duration" .= eval_duration
        ]

-- ストリーミングのコールバック関数の型
type ChatStreamCallback = ChatMessage -> Bool -> IO ()

---
--- ChatRequest を ollama api に送信してレスポンスを受け取る
---
chat :: ChatRequest -> Text -> IO (Either Text ChatResponse)
chat chatReq endpoint = do 
    req' <- parseRequest $ T.unpack (endpoint <> "/api/chat")
    let notStreaming = chatReq { reqStream = False } -- ストリーミングを無効化
    let requestBody = encode notStreaming
    let request = setRequestMethod "POST" 
            $ setRequestHeader "Content-Type" ["application/json"] 
            $ setRequestBodyLBS requestBody req'
    response <- httpLBS request
    let body = getResponseBody response
    let output = decode body :: Maybe ChatResponse
    case output of
        Nothing -> return $ Left ("Failed to decode response: " <> E.decodeUtf8 (L8.toStrict body))
        Just o -> return $ Right o
      

--- テキスト入力から簡単なChatRequestを作成するヘルパー関数
--- 
chatWithText :: Text -> Text -> Text -> IO (Either Text ChatResponse)
chatWithText ollamaModel input = chat $ ChatRequest
  { reqModel = ollamaModel
  , reqMessages = [ChatMessage { role = "user", content = input }]
  , reqStream = False
  }