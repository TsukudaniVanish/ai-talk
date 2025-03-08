{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}
module VoiceBox 
  ( Speaker(..)
  , Style(..)
  , StyleType(..)
  , SupportedFeatures(..)
  , PermittedSynthesisMorphing(..)
  , getSpeakers
  , Mora(..)
  , AccentPhrase(..)
  , AudioQuery(..)
  , getAudioQuery
  , synthesisAudio  -- エクスポートリストに追加
  ) where

import Pre 
import Network.HTTP.Simple
import Data.Aeson
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import Data.Text.Encoding (encodeUtf8)
import Control.Exception (try)

-- スタイルタイプの定義
data StyleType = Talk | Singing
  deriving (Show, Eq)

instance FromJSON StyleType where
  parseJSON = withText "StyleType" $ \v -> case v of
    "talk" -> pure Talk
    "singing" -> pure Singing
    _ -> fail $ "Unknown style type: " ++ T.unpack v

-- スピーカースタイルを表現するデータ型
data Style = Style
  { styleName :: T.Text
  , styleId :: Int
  , styleType :: StyleType
  } deriving (Show, Eq)

instance FromJSON Style where
  parseJSON = withObject "Style" $ \v -> Style
    <$> v .: "name"
    <*> v .: "id"
    <*> v .: "type"

-- 合成モーフィング許可設定
data PermittedSynthesisMorphing = All | SpeakerOnly | NoMorphing
  deriving (Show, Eq)

instance FromJSON PermittedSynthesisMorphing where
  parseJSON = withText "PermittedSynthesisMorphing" $ \v -> case v of
    "ALL" -> pure All
    "SPEAKER_ONLY" -> pure SpeakerOnly
    "NOTHING" -> pure NoMorphing
    _ -> fail $ "Unknown morphing permission: " ++ T.unpack v

-- サポートされている機能
data SupportedFeatures = SupportedFeatures
  { permittedSynthesisMorphing :: PermittedSynthesisMorphing
  } deriving (Show, Eq)

instance FromJSON SupportedFeatures where
  parseJSON = withObject "SupportedFeatures" $ \v -> SupportedFeatures
    <$> v .: "permitted_synthesis_morphing"

-- スピーカーを表現するデータ型
data Speaker = Speaker
  { speakerName :: T.Text
  , speakerUuid :: T.Text
  , speakerStyles :: [Style]
  , speakerVersion :: T.Text
  , speakerSupportedFeatures :: SupportedFeatures
  } deriving (Show, Eq)

-- JSONからSpeakerへのパース方法を定義
instance FromJSON Speaker where
  parseJSON = withObject "Speaker" $ \v -> Speaker
    <$> v .: "name"
    <*> v .: "speaker_uuid"
    <*> v .: "styles"
    <*> v .: "version"
    <*> v .: "supported_features"

-- APIからスピーカー情報を取得する関数
-- baseUrl: APIのベースURL (例: "https://api.example.com")
getSpeakers :: T.Text -> Maybe T.Text -> IO (Either T.Text [Speaker])
getSpeakers baseUrl coreVersion = do
  -- APIリクエストを作成 (必要に応じてクエリパラメータを追加)
  let endpoint = "/speakers"
      urlStr = T.unpack $ baseUrl <> endpoint
      request = case coreVersion of
        Nothing -> parseRequest_ $ "GET " <> urlStr
        Just ver -> setRequestQueryString [("core_version", Just (encodeUtf8 ver))] $ parseRequest_ $ "GET " <> urlStr
  
  -- リクエストを実行し、レスポンスを取得
  response <- try $ httpLBS request
  
  -- レスポンスの処理
  case response of
    Left e -> pure $ Left $ T.pack $ "HTTP request failed: " ++ show (e :: HttpException)
    Right res -> do
      let status = getResponseStatusCode res
      if status == 200
        then case eitherDecode (getResponseBody res) of
               Left err -> pure $ Left $ T.pack $ "JSON parse error: " ++ err
               Right speakers -> pure $ Right speakers
        else if status == 422
             then pure $ Left "Validation error: Invalid parameters"
             else pure $ Left $ T.pack $ "API returned status code: " ++ show status

-- モーラ（音節）を表現するデータ型
data Mora = Mora
  { moraText :: T.Text
  , moraConsonant :: Maybe T.Text
  , moraConsonantLength :: Maybe Double
  , moraVowel :: T.Text
  , moraVowelLength :: Double
  , moraPitch :: Double
  } deriving (Show, Eq)

instance FromJSON Mora where
  parseJSON = withObject "Mora" $ \v -> Mora
    <$> v .: "text"
    <*> v .:? "consonant"
    <*> v .:? "consonant_length"
    <*> v .: "vowel"
    <*> v .: "vowel_length"
    <*> v .: "pitch"

-- ToJSON インスタンスの実装
instance ToJSON Mora where
  toJSON mora = object
    [ "text" .= moraText mora
    , "consonant" .= moraConsonant mora
    , "consonant_length" .= moraConsonantLength mora
    , "vowel" .= moraVowel mora
    , "vowel_length" .= moraVowelLength mora
    , "pitch" .= moraPitch mora
    ]

-- アクセント句を表現するデータ型
data AccentPhrase = AccentPhrase
  { accentMoras :: [Mora]
  , accentAccent :: Int
  , accentPauseMora :: Maybe Mora
  , accentIsInterrogative :: Bool
  } deriving (Show, Eq)

instance FromJSON AccentPhrase where
  parseJSON = withObject "AccentPhrase" $ \v -> AccentPhrase
    <$> v .: "moras"
    <*> v .: "accent"
    <*> v .:? "pause_mora"
    <*> v .: "is_interrogative"

-- ToJSON インスタンスの実装
instance ToJSON AccentPhrase where
  toJSON phrase = object
    [ "moras" .= accentMoras phrase
    , "accent" .= accentAccent phrase
    , "pause_mora" .= accentPauseMora phrase
    , "is_interrogative" .= accentIsInterrogative phrase
    ]

-- 音声合成クエリを表現するデータ型
data AudioQuery = AudioQuery
  { audioQueryAccentPhrases :: [AccentPhrase]
  , audioQuerySpeedScale :: Double
  , audioQueryPitchScale :: Double
  , audioQueryIntonationScale :: Double
  , audioQueryVolumeScale :: Double
  , audioQueryPrePhonemeLength :: Double
  , audioQueryPostPhonemeLength :: Double
  , audioQueryPauseLength :: Double
  , audioQueryPauseLengthScale :: Double
  , audioQueryOutputSamplingRate :: Int
  , audioQueryOutputStereo :: Bool
  , audioQueryKana :: Maybe T.Text
  } deriving (Show, Eq)

instance FromJSON AudioQuery where
  parseJSON = withObject "AudioQuery" $ \v -> AudioQuery
    <$> v .: "accent_phrases"
    <*> v .: "speedScale"
    <*> v .: "pitchScale"
    <*> v .: "intonationScale"
    <*> v .: "volumeScale"
    <*> v .: "prePhonemeLength"
    <*> v .: "postPhonemeLength"
    <*> v .: "pauseLength"
    <*> v .:? "pauseLengthScale" .!= 1.0
    <*> v .: "outputSamplingRate"
    <*> v .: "outputStereo"
    <*> v .:? "kana"

-- ToJSON インスタンスの実装
instance ToJSON AudioQuery where
  toJSON query = object
    [ "accent_phrases" .= audioQueryAccentPhrases query
    , "speedScale" .= audioQuerySpeedScale query
    , "pitchScale" .= audioQueryPitchScale query
    , "intonationScale" .= audioQueryIntonationScale query
    , "volumeScale" .= audioQueryVolumeScale query
    , "prePhonemeLength" .= audioQueryPrePhonemeLength query
    , "postPhonemeLength" .= audioQueryPostPhonemeLength query
    , "pauseLength" .= audioQueryPauseLength query
    , "pauseLengthScale" .= audioQueryPauseLengthScale query
    , "outputSamplingRate" .= audioQueryOutputSamplingRate query
    , "outputStereo" .= audioQueryOutputStereo query
    , "kana" .= audioQueryKana query
    ]

-- 音声合成用のクエリを作成する関数
getAudioQuery :: T.Text -> T.Text -> Int -> Maybe T.Text -> IO (Either T.Text AudioQuery)
getAudioQuery baseUrl text speakerId coreVersion = do
  -- APIリクエストを作成
  let endpoint = "/audio_query"
      urlStr = T.unpack $ baseUrl <> endpoint
      baseRequest = parseRequest_ $ "POST " <> urlStr
      
      -- 必須クエリパラメータを設定
      reqWithParams = setRequestQueryString
        [ ("text", Just (encodeUtf8 text))
        , ("speaker", Just (encodeUtf8 $ T.pack $ show speakerId))
        ] baseRequest
      
      -- オプションのcoreVersionを設定（指定があれば）
      request = case coreVersion of
        Nothing -> reqWithParams
        Just ver -> setRequestQueryString [("core_version", Just (encodeUtf8 ver))] reqWithParams
  
  -- リクエストを実行し、レスポンスを取得
  response <- try $ httpLBS request
  
  -- レスポンスの処理
  case response of
    Left e -> pure $ Left $ T.pack $ "HTTP request failed: " ++ show (e :: HttpException)
    Right res -> do
      let status = getResponseStatusCode res
      if status == 200
        then case eitherDecode (getResponseBody res) of
               Left err -> pure $ Left $ T.pack $ "JSON parse error: " ++ err
               Right query -> pure $ Right query
        else if status == 422
             then pure $ Left "Validation error: Invalid parameters"
             else pure $ Left $ T.pack $ "API returned status code: " ++ show status

-- 音声合成を実行する関数
synthesisAudio :: T.Text -> AudioQuery -> Int -> Maybe Bool -> Maybe T.Text -> IO (Either T.Text BL.ByteString)
synthesisAudio baseUrl query speakerId enableInterrogativeUpspeak coreVersion = do
  -- APIリクエストを作成
  let endpoint = "/synthesis"
      urlStr = T.unpack $ baseUrl <> endpoint
      baseRequest = parseRequest_ $ "POST " <> urlStr

      -- 必須クエリパラメータを設定
      reqWithParams = setRequestQueryString
        [ ("speaker", Just (encodeUtf8 $ T.pack $ show speakerId))
        ] baseRequest
      
      -- オプションのクエリパラメータを設定
      reqWithInterrogative = case enableInterrogativeUpspeak of
        Nothing -> reqWithParams
        Just enable -> setRequestQueryString [("enable_interrogative_upspeak", Just (encodeUtf8 $ T.pack $ show enable))] reqWithParams

      -- オプションのcoreVersionを設定
      reqWithCoreVersion = case coreVersion of
        Nothing -> reqWithInterrogative
        Just ver -> setRequestQueryString [("core_version", Just (encodeUtf8 ver))] reqWithInterrogative
      
      -- リクエストボディとヘッダーを設定
      request = setRequestBodyJSON query
              $ setRequestHeader "Content-Type" ["application/json"]
              $ setRequestHeader "Accept" ["audio/wav"]
                reqWithCoreVersion
  
  -- リクエストを実行し、レスポンスを取得
  response <- try $ httpLBS request
  
  -- レスポンスの処理
  case response of
    Left e -> pure $ Left $ T.pack $ "HTTP request failed: " ++ show (e :: HttpException)
    Right res -> do
      let status = getResponseStatusCode res
      if status == 200
        then pure $ Right $ getResponseBody res
        else if status == 422
             then pure $ Left "Validation error: Invalid parameters"
             else pure $ Left $ T.pack $ "API returned status code: " ++ show status

