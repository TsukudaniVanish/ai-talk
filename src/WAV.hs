{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module WAV
  ( playWAV,
    playWAVFile,
    playWavOpenAL,
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, catch)
import Control.Monad (when)
import Data.Bits (Bits (shiftL))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Unsafe (unsafeUseAsCString)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Foreign.C.String (CString)
import Pre
import Sound.OpenAL
  ( BufferData (BufferData),
    Format (..),
    GeneratableObjectName (genObjectName),
    HasGetter (get),
    HasSetter (($=)),
    MemoryRegion (MemoryRegion),
    ObjectName (deleteObjectName),
    Source,
    SourceState (Playing),
    alErrors,
    bufferData,
    closeDevice,
    createContext,
    currentContext,
    destroyContext,
    openDevice,
    play,
    queueBuffers,
    sourceState,
  )
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import qualified System.Info as SysInfo
import System.Process.ByteString.Lazy (readProcessWithExitCode)

-- | Save WAV data to a temporary file and play it
playWAV :: BL.ByteString -> IO Bool
playWAV wavData = do
  withSystemTempFile "temp.wav" $ \filePath handle -> do
    BL.hPut handle wavData
    hClose handle
    playWAVFile filePath `catch` handlePlayError
  where
    handlePlayError :: SomeException -> IO Bool
    handlePlayError _ = do
      BL.putStr "Error occurred while playing WAV file\n"
      return False

-- | Play a WAV file
playWAVFile :: FilePath -> IO Bool
playWAVFile filePath = do
  let cmd = case os of
        "windows" -> "powershell"
        "darwin" -> "afplay"
        _ -> "aplay" -- Linux
      args = case os of
        "windows" -> ["-c", "(New-Object System.Media.SoundPlayer '" ++ filePath ++ "').PlaySync()"]
        _ -> [filePath]

  (exitCode, _, stderr) <- readProcessWithExitCode cmd args BL.empty
  if BL.null stderr
    then return True
    else do
      BL.putStr ("Error: " <> stderr <> "\n")
      return False

-- | Get current OS name
os :: String
os = case SysInfo.os of
  "mingw32" -> "windows"
  "darwin" -> "darwin"
  _ -> "linux"

-- | Play WAV data using OpenAL directly from ByteString without temporary files
playWavOpenAL :: BL.ByteString -> IO Bool
playWavOpenAL wavData = do
  -- Convert ByteString.Lazy to ByteString (strict)
  let bs = BS.concat $ BL.toChunks wavData
  case parseWAVHeader bs of
    Left err -> do
      BS.putStr $ "Error occurred while parsing WAV header: " <> encodeUtf8 (T.pack err) <> "\n"
      return False
    Right (format, sampleRate, _, _, dataOffset, dataSize) -> unsafeUseAsCString (BS.drop dataOffset bs) $ \ptr -> do
      -- unsafeUseAsCString releases ptr when done, so we need to perform playback here
      playWavOpenALDirect ptr format sampleRate dataSize

playWavOpenALDirect :: CString -> Format -> Int -> Int -> IO Bool
playWavOpenALDirect ptr format sampleRate dataSize = do
  -- Initialize OpenAL context
  (Just device) <- openDevice Nothing
  (Just context) <- createContext device []
  currentContext $= Just context

  -- Create buffer and source
  buffer <- genObjectName
  source <- genObjectName

  let memoryRegion = MemoryRegion ptr (fromIntegral dataSize)

  -- Load data into buffer
  bufferData buffer $= BufferData memoryRegion format (fromIntegral sampleRate)
  errs <- get alErrors
  when (null errs) $ do
    -- Associate buffer with source
    queueBuffers source [buffer]

    play [source]

    -- Wait for playback to complete
    waitForSourceDirect source

  -- Cleanup
  deleteObjectName source
  deleteObjectName buffer

  -- Close context
  currentContext $= Nothing
  destroyContext context
  closeDevice device
  where
    -- Wait until source finishes playing
    waitForSourceDirect :: Source -> IO ()
    waitForSourceDirect source = do
      state <- get (sourceState source)
      when (state == Playing) $ do
        threadDelay 100000 -- 100ms
        waitForSourceDirect source

-- | Parse WAV header
parseWAVHeader :: BS.ByteString -> Either String (Format, Int, Int, Int, Int, Int)
parseWAVHeader bs
  | BS.length bs < 44 = Left "WAV file is too short"
  | not (BS.isPrefixOf "RIFF" bs) = Left "Missing RIFF signature"
  | not (BS.isPrefixOf "WAVE" (BS.drop 8 bs)) = Left "Not a WAVE format"
  | not (BS.isPrefixOf "fmt " (BS.drop 12 bs)) = Left "Missing fmt chunk"
  | otherwise = do
      -- Read the following values from byte array
      let audioFormat = getInt16 bs 20 -- Format type (1 = PCM)
          numChannels = getInt16 bs 22 -- Number of channels (1 = mono, 2 = stereo)
          sampleRate = getInt32 bs 24 -- Sample rate (Hz)
          bitsPerSample = getInt16 bs 34 -- Bit depth (8, 16, etc.)

      -- Find "data" chunk
      case findDataChunk bs 36 of
        Nothing -> Left "Data chunk not found"
        Just (dataOffset, dataSize) -> do
          -- Check and convert audio format
          if audioFormat /= 1
            then Left "Only PCM format is supported"
            else
              let format = determineALFormat numChannels bitsPerSample
               in Right (format, sampleRate, numChannels, bitsPerSample, dataOffset, dataSize)

-- | Find position and size of "data" chunk
findDataChunk :: BS.ByteString -> Int -> Maybe (Int, Int)
findDataChunk bs offset
  | offset + 8 > BS.length bs = Nothing
  | BS.isPrefixOf "data" (BS.drop offset bs) =
      let dataSize = getInt32 bs (offset + 4)
          dataOffset = offset + 8
       in Just (dataOffset, dataSize)
  | otherwise =
      let chunkSize = getInt32 bs (offset + 4)
          nextOffset = offset + 8 + chunkSize
       in findDataChunk bs nextOffset

-- | Determine OpenAL format from channel count and bit depth
determineALFormat :: Int -> Int -> Format
determineALFormat channels bitsPerSample = case (channels, bitsPerSample) of
  (1, 8) -> Mono8
  (1, 16) -> Mono16
  (2, 8) -> Stereo8
  (2, 16) -> Stereo16
  _ ->
    error
      $ "Unsupported audio format: "
      <> "channels="
      <> show channels
      <> ", bit depth="
      <> show bitsPerSample

-- | Get 16-bit integer value from byte array (little endian)
getInt16 :: BS.ByteString -> Int -> Int
getInt16 bs offset =
  let b1 = fromIntegral $ BS.index bs offset
      b2 = fromIntegral $ BS.index bs (offset + 1)
   in b1 + (b2 `shiftL` 8)

-- | Get 32-bit integer value from byte array (little endian)
getInt32 :: BS.ByteString -> Int -> Int
getInt32 bs offset =
  let b1 = fromIntegral $ BS.index bs offset
      b2 = fromIntegral $ BS.index bs (offset + 1)
      b3 = fromIntegral $ BS.index bs (offset + 2)
      b4 = fromIntegral $ BS.index bs (offset + 3)
   in b1 + (b2 `shiftL` 8) + (b3 `shiftL` 16) + (b4 `shiftL` 24)