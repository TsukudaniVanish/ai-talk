{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Infrastructures.ControllerIO
  ( ControllerIO (..),
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text.IO as T
import Pre
import System.IO (hFlush, stdout)
import Usecase.Controller (IController (..), UserInput (..))
import WAV (playWavOpenAL)

newtype ControllerIO a = ControllerIO
  { runControllerIO :: IO a
  }

instance Functor ControllerIO where
  fmap f (ControllerIO x) = ControllerIO $ f <$> x

instance Applicative ControllerIO where
  pure = ControllerIO . pure
  ControllerIO f <*> ControllerIO x = ControllerIO $ f <*> x

instance Monad ControllerIO where
  ControllerIO x >>= f = ControllerIO $ x >>= runControllerIO . f

instance MonadIO ControllerIO where
  liftIO = ControllerIO

instance IController ControllerIO where
  getUserInput = do
    liftIO $ T.putStr "You: "
    liftIO $ hFlush stdout
    input <- liftIO $ T.getLine
    case input of
      q | q == ":q" || q == ":quit" -> return Quit
      _ -> return $ Message input

  printMessage = liftIO . T.putStrLn

  playWav = liftIO . playWavOpenAL