{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Usecase.AIChatService
  ( AITalkConfig (..),
    ChatCharacter (..),
    ChatPiece (..),
    IAIChartService (..),
    processMessage,
    pack,
    unpack,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import qualified Data.Text.IO as T
import Pre

data AITalkConfig = AITalkConfig
  { ollamaUrl :: Text,
    ollamaModel :: Text,
    voiceBoxUrl :: Text,
    voiceBoxStyleId :: Maybe Int
  }

data ChatCharacter = User | Assistant
  deriving (Show)

unpack :: ChatCharacter -> Text
unpack User = "user"
unpack Assistant = "assistant"

pack :: Text -> Either Text ChatCharacter
pack "user" = Right User
pack "assistant" = Right Assistant
pack _ = Left "Invalid character"

data ChatPiece = ChatPiece
  { character :: ChatCharacter,
    script :: Text
  }
  deriving (Show)

class (MonadIO m) => IAIChartService m where
  -- | Generate a response message from AI. If succeeded, it updates the chat history.
  generateMessage :: Text -> m (Either Text Text)

  -- | Play a message using the VoiceBox service
  playMessage :: Text -> m (Either Text ())

  -- | Fetch chat history
  fetchChatHistory :: m [ChatPiece]

  -- | Push a chat piece to the chat history
  pushChatPieceToHistory :: ChatPiece -> m ()

processMessage :: (IAIChartService m) => Text -> m ()
processMessage input = do
  aiResponse <- generateMessage input
  case aiResponse of
    Left err -> liftIO $ T.putStrLn err
    Right response -> do
      liftIO $ T.putStrLn response
      playResult <- playMessage response
      case playResult of
        Left err -> liftIO $ T.putStrLn err
        Right _ -> return ()