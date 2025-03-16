{-# LANGUAGE NoImplicitPrelude #-}

module Infrastructures.ChatIO
  ( ChatIO (..),
    ChatIOState (..),
    getState,
    toChatMessage,
  )
where

import Chat (ChatMessage (..), ChatRequest (..), chat, resMessage)
import Control.Monad.State
import Data.Text (Text)
import Entity.ChatPiece (ChatPiece (..), unpack)
import Pre
import Usecase.Chat (IChatService (..))

data ChatIOState = ChatIOSate
  { ollamaModel :: Text,
    ollamaUrl :: Text
  }

data ChatIO a = ChatIO
  { runChatIO :: StateT ChatIOState IO a
  }

instance Functor ChatIO where
  fmap f (ChatIO x) = ChatIO $ fmap f x

instance Applicative ChatIO where
  pure = ChatIO . pure
  ChatIO f <*> ChatIO x = ChatIO $ f <*> x

instance Monad ChatIO where
  ChatIO x >>= f = ChatIO $ x >>= runChatIO . f

instance MonadIO ChatIO where
  liftIO = ChatIO . liftIO

getState :: ChatIO ChatIOState
getState = ChatIO get

toChatMessage :: ChatPiece -> ChatMessage
toChatMessage cp = ChatMessage (unpack $ character cp) (script cp)

instance IChatService (ChatIO) where
  chatResponse chats = do
    s <- getState
    let cms = map toChatMessage chats
        req = ChatRequest (ollamaModel s) cms False
    o <- liftIO $ chat req (ollamaUrl s)
    case o of
      Left e -> return $ Left e
      Right res -> do
        let botMessage = resMessage res
            botContent = content botMessage
        return $ Right botContent