{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module WAV 
  ( playWAV
  , playWAVFile
  ) where 

import Pre
import qualified Data.ByteString.Lazy as BL
import qualified System.Info as SysInfo
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import System.Process.ByteString.Lazy (readProcessWithExitCode)
import Control.Exception (catch, SomeException)

-- | WAVデータを一時ファイルに保存し再生する
playWAV :: BL.ByteString -> IO Bool
playWAV wavData = do
  withSystemTempFile "temp.wav" $ \filePath handle -> do
    BL.hPut handle wavData
    hClose handle
    playWAVFile filePath `catch` handlePlayError
  where
    handlePlayError :: SomeException -> IO Bool
    handlePlayError _ = do
      BL.putStr  "WAVファイルの再生中にエラーが発生しました\n"
      return False

-- | WAVファイルを再生する
playWAVFile :: FilePath -> IO Bool
playWAVFile filePath = do
  let
    cmd = case os of
      "windows" -> "powershell"
      "darwin"  -> "afplay"
      _         -> "aplay"  -- Linux
    
    args = case os of
      "windows" -> ["-c", "(New-Object System.Media.SoundPlayer '" ++ filePath ++ "').PlaySync()"]
      _         -> [filePath]
  
  (exitCode, _, stderr) <- readProcessWithExitCode cmd args BL.empty
  if BL.null stderr
    then return True
    else do
      BL.putStr ("エラー: " <> stderr <> "\n")
      return False

-- | 現在のOS名を取得
os :: String
os = case SysInfo.os of
  "mingw32" -> "windows"
  "darwin"  -> "darwin"
  _         -> "linux"