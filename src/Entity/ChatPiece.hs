{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Entity.ChatPiece where

import Data.Text (Text)
import Pre

data ChatCharacter = User | Assistant
  deriving (Show)

unpack :: ChatCharacter -> Text
unpack User = "user"
unpack Assistant = "assistant"

pack :: Text -> Either Text ChatCharacter
pack "user" = Right User
pack "assistant" = Right Assistant
pack _ = Left "Invalid character"

data ChatPiece = ChatPiece
  { character :: ChatCharacter,
    script :: Text
  }
  deriving (Show)
