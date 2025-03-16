{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Usecase.AIChatService2 where

import Data.Text (Text)
import Entity.ChatPiece (ChatCharacter (..), ChatPiece (..))
import Pre
import Usecase.Chat (IChatService (..))
import Usecase.Controller (IController (..), UserInput (..))
import Usecase.Repository (IRepository (..))
import Usecase.TTS (ITextToSpeech (..))

-- \| Generate a response message from AI. If succeeded, it updates the chat history.
generateMessage :: (IChatService m, IRepository m) => Text -> m (Either Text Text)
generateMessage input = do
  history <- fetchChatHistory
  aiResponse <- chatResponse (history ++ [ChatPiece User input])
  case aiResponse of
    Left err -> return $ Left err
    Right response -> do
      pushChatPieceToHistory (ChatPiece User input)
      pushChatPieceToHistory (ChatPiece Assistant response)
      return $ Right response

-- | Play a message using the VoiceBox service
playMessage :: (IController m, ITextToSpeech m) => Text -> m (Either Text ())
playMessage response = do
  ttsResponse <- textToSpeech response
  case ttsResponse of
    Left err -> return $ Left err
    Right wav -> do
      playResult <- playWav wav
      if playResult
        then return $ Right ()
        else return $ Left "Failed to play the message."

processMessage :: (IController m, IRepository m, ITextToSpeech m, IChatService m) => Text -> m ()
processMessage input = do
  aiResponse <- generateMessage input
  case aiResponse of
    Left err -> printMessage err
    Right response -> do
      printMessage response
      playResult <- playMessage response
      case playResult of
        Left err -> printMessage err
        Right _ -> return ()

-- | main loop for the AI chat service
runAIChatService :: (IChatService m, IRepository m, IController m, ITextToSpeech m) => m ()
runAIChatService = do
  input <- getUserInput
  case input of
    Quit -> return ()
    Message msg -> do
      processMessage msg
      runAIChatService
