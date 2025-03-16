{-# LANGUAGE NoImplicitPrelude #-}

module Usecase.Repository
  ( IRepository (..),
  )
where

import Control.Monad.IO.Class (MonadIO)
import Entity.ChatPiece (ChatPiece)

class (MonadIO m) => IRepository m where
  fetchChatHistory :: m [ChatPiece]
  pushChatPieceToHistory :: ChatPiece -> m ()
