{-# LANGUAGE NoImplicitPrelude #-}

module Interfaces.Repository
  ( Repository (..),
  )
where

import Control.Monad.State
import Entity.ChatPiece (ChatPiece)
import Pre
import Usecase.Repository (IRepository (..))

newtype (MonadIO m) => Repository m a = Repository
  { runRepository :: StateT [ChatPiece] m a
  }

instance (MonadIO m) => Functor (Repository m) where
  fmap f (Repository x) = Repository $ f <$> x

instance (MonadIO m) => Applicative (Repository m) where
  pure = Repository . pure
  Repository f <*> Repository x = Repository $ f <*> x

instance (MonadIO m) => Monad (Repository m) where
  Repository x >>= f = Repository $ x >>= runRepository . f

instance (MonadIO m) => MonadIO (Repository m) where
  liftIO = Repository . liftIO

instance (MonadIO m) => IRepository (Repository m) where
  fetchChatHistory = Repository get
  pushChatPieceToHistory cp = Repository $ do
    history <- get
    put $ cp : history
    return ()
