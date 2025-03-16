{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Usecase.Chat
  ( IChatService (..),
  )
where

import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import Entity.ChatPiece (ChatPiece (..))
import Pre

class (MonadIO m) => IChatService m where
  -- | Generate a response message from AI.
  chatResponse :: [ChatPiece] -> m (Either Text Text)
