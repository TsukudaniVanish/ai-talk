{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Usecase.Repository
  ( IRepository (..),
  )
where

import Control.Monad.IO.Class (MonadIO)
import Entity.ChatPiece (ChatPiece)
import Pre

class (MonadIO m) => IRepository m where
  fetchChatHistory :: m [ChatPiece]
  pushChatPieceToHistory :: ChatPiece -> m ()
