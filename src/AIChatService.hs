{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AIChatService
  ( AIChatService (..),
    ChatCharacter (..),
    ChatPiece (..),
    fromChatMessage,
    getServiceState,
    toChatMessage,
  )
where

import Chat (ChatMessage (..), ChatRequest (..), ChatResponse (..), chat)
import Control.Monad.State
import Data.Text (Text)
import Pre
import Usecase.AIChatService
  ( AITalkConfig (..),
    ChatCharacter (..),
    ChatPiece (..),
    IAIChartService (..),
    pack,
    unpack,
  )
import VoiceBox (textToSpeech)
import WAV (playWavOpenAL)

toChatMessage :: ChatPiece -> ChatMessage
toChatMessage cp = ChatMessage (unpack $ character cp) (script cp)

fromChatMessage :: ChatMessage -> Either Text ChatPiece
fromChatMessage cm = do
  c <- pack $ role cm
  return $ ChatPiece c (content cm)

data AIChatService a = AIChatService
  { runAIChatService :: StateT ([ChatPiece], AITalkConfig) IO a
  }

getServiceState :: AIChatService ([ChatPiece], AITalkConfig)
getServiceState = AIChatService get

instance Functor AIChatService where
  fmap f mx = AIChatService $ do
    x <- runAIChatService mx
    return (f x)

instance Applicative AIChatService where
  pure x = AIChatService $ return x
  mf <*> mx = AIChatService $ do
    f <- runAIChatService mf
    x <- runAIChatService mx
    return $ f x

instance Monad AIChatService where
  return = pure
  mx >>= f = AIChatService $ do
    x <- runAIChatService mx
    runAIChatService $ f x

instance MonadIO AIChatService where
  liftIO mx = AIChatService $ liftIO mx

instance IAIChartService AIChatService where
  fetchChatHistory = AIChatService $ do
    (history, _) <- get
    return history
  pushChatPieceToHistory cp = AIChatService $ do
    (history, config) <- get
    put (history ++ [cp], config)

  generateMessage input = do
    (history, config) <- getServiceState
    let messages = map toChatMessage history
        req = ChatRequest (ollamaModel config) (messages ++ [ChatMessage "user" input]) False
    o <- liftIO $ chat req (ollamaUrl config)
    case o of
      Left e -> return $ Left e
      Right r -> do
        let botMessage = resMessage r
            botContent = content botMessage
        pushChatPieceToHistory $ ChatPiece User input
        pushChatPieceToHistory $ ChatPiece Assistant botContent
        return $ Right botContent

  playMessage msg = do
    (_, config) <- getServiceState
    case voiceBoxStyleId config of
      Nothing -> return $ Right ()
      Just styleId -> do
        audioResult <- liftIO $ textToSpeech (voiceBoxUrl config) msg styleId Nothing
        case audioResult of
          Left err -> return $ Left err
          Right wavData -> do
            playResult <- liftIO $ playWavOpenAL wavData
            return $ if playResult then Right () else Left "Failed to play audio"
