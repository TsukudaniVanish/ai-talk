{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module CUI
  ( CUICommand (..),
    getUserInput,
    parseUserInput,
  )
where

import Data.Text (Text)
import qualified Data.Text.IO as T
import Pre
import System.IO (hFlush, stdout)

data CUICommand
  = QuitCommand
  | MessageCommand Text

-- | Get user input from the console
getUserInput :: IO Text
getUserInput = do
  T.putStr "You: "
  hFlush stdout
  T.getLine

-- | Parse user input to determine the command type
parseUserInput :: Text -> CUICommand
parseUserInput input
  | input == ":q" || input == ":quit" = QuitCommand
  | otherwise = MessageCommand input