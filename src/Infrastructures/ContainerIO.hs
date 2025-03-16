{-# LANGUAGE NoImplicitPrelude #-}

module Infrastructures.ContainerIO
  ( ContainerIO (..),
    ContainerIOState (..),
    getContainerIOState,
    liftChatIO,
    liftController,
    liftRepository,
    liftTTSIO,
  )
where

import Control.Monad.State
import Data.Text (Text)
import Entity.ChatPiece (ChatPiece (..))
import Infrastructures.ChatIO (ChatIO (..), ChatIOState (ChatIOSate))
import Infrastructures.ControllerIO (ControllerIO (..))
import Infrastructures.TTSIO (TTSIO (..), TTSIOSate (TTSIOSate))
import Interfaces.Repository (Repository (..))
import Pre
import Usecase.Chat (IChatService (..))
import Usecase.Controller (IController (..))
import Usecase.Repository (IRepository (..))
import Usecase.TTS (ITextToSpeech (..))

data ContainerIOState = ContainerIOState
  { ollamaUrl :: Text,
    ollamaModel :: Text,
    voiceBoxUrl :: Text,
    voiceBoxStyleId :: Maybe Int,
    history :: [ChatPiece]
  }

data ContainerIO a = ContainerIO
  { runContainerIO :: StateT ContainerIOState IO a
  }

instance Functor ContainerIO where
  fmap f mx = ContainerIO $ do
    x <- runContainerIO mx
    return (f x)

instance Applicative ContainerIO where
  pure x = ContainerIO $ return x
  mf <*> mx = ContainerIO $ do
    f <- runContainerIO mf
    x <- runContainerIO mx
    return $ f x

instance Monad ContainerIO where
  return = pure
  mx >>= f = ContainerIO $ do
    x <- runContainerIO mx
    runContainerIO $ f x

instance MonadIO ContainerIO where
  liftIO = ContainerIO . liftIO

getContainerIOState :: ContainerIO ContainerIOState
getContainerIOState = ContainerIO get

liftController :: ControllerIO a -> ContainerIO a
liftController (ControllerIO c) = ContainerIO $ do
  s <- get
  (a, s'') <- liftIO $ runStateT (StateT $ \s' -> do x <- c; return (x, s')) s
  put s''
  return a

instance IController ContainerIO where
  getUserInput = liftController getUserInput
  playWav = liftController . playWav
  printMessage = liftController . printMessage

liftRepository :: Repository IO a -> ContainerIO a
liftRepository (Repository r) = ContainerIO $ do
  s <- get
  (a, s'') <- liftIO $ runStateT (StateT $ \s' -> do (a, hist) <- runStateT r (history s'); return (a, ContainerIOState (ollamaUrl s') (ollamaModel s') (voiceBoxUrl s') (voiceBoxStyleId s') hist)) s
  put s''
  return a

instance IRepository ContainerIO where
  fetchChatHistory = liftRepository fetchChatHistory
  pushChatPieceToHistory = liftRepository . pushChatPieceToHistory

liftChatIO :: ChatIO a -> ContainerIO a
liftChatIO (ChatIO c) = ContainerIO $ do
  s <- get
  (a, s'') <- liftIO $ runStateT (StateT $ \s' -> do (a, ChatIOSate m u) <- runStateT c (ChatIOSate (ollamaModel s') (ollamaUrl s')); return (a, ContainerIOState u m (voiceBoxUrl s') (voiceBoxStyleId s') (history s'))) s
  put s''
  return a

instance IChatService ContainerIO where
  chatResponse = liftChatIO . chatResponse

liftTTSIO :: TTSIO a -> ContainerIO a
liftTTSIO (TTSIO c) = ContainerIO $ do
  s <- get
  (a, s'') <- liftIO $ runStateT (StateT $ \s' -> do (a, TTSIOSate vbSId vbURL) <- runStateT c (TTSIOSate (voiceBoxStyleId s') (voiceBoxUrl s')); return (a, ContainerIOState (ollamaUrl s') (ollamaModel s') (vbURL) (vbSId) (history s'))) s
  put s''
  return a

instance ITextToSpeech ContainerIO where
  textToSpeech = liftTTSIO . textToSpeech