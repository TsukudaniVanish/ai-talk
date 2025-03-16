{-# LANGUAGE NoImplicitPrelude #-}

module Usecase.Controller
  ( UserInput (..),
    IController (..),
  )
where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import Pre

data UserInput = Quit | Message Text
  deriving (Show)

class (MonadIO m) => IController m where
  getUserInput :: m UserInput
  playWav :: BL.ByteString -> m Bool
  printMessage :: Text -> m ()