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
  , synthesisAudio
  , findSpeakerStyle
  , textToSpeech
  ) where

import Pre 
import Network.HTTP.Simple
import Data.Aeson
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import Data.Text.Encoding (encodeUtf8)
import Control.Exception (try)
import Data.List (find)

-- Definition of style type
data StyleType = Talk | Singing
  deriving (Show, Eq)

instance FromJSON StyleType where
  parseJSON = withText "StyleType" $ \v -> case v of
    "talk" -> pure Talk
    "singing" -> pure Singing
    _ -> fail $ "Unknown style type: " ++ T.unpack v

-- Data type representing a speaker style
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

-- Permitted synthesis morphing settings
data PermittedSynthesisMorphing = All | SelfOnly | NoMorphing
  deriving (Show, Eq)

instance FromJSON PermittedSynthesisMorphing where
  parseJSON = withText "PermittedSynthesisMorphing" $ \v -> case v of
    "ALL" -> pure All
    "SELF_ONLY" -> pure SelfOnly
    "NOTHING" -> pure NoMorphing
    _ -> fail $ "Unknown morphing permission: " ++ T.unpack v

-- Supported features
data SupportedFeatures = SupportedFeatures
  { permittedSynthesisMorphing :: PermittedSynthesisMorphing
  } deriving (Show, Eq)

instance FromJSON SupportedFeatures where
  parseJSON = withObject "SupportedFeatures" $ \v -> SupportedFeatures
    <$> v .: "permitted_synthesis_morphing"

-- Data type representing a speaker
data Speaker = Speaker
  { speakerName :: T.Text
  , speakerUuid :: T.Text
  , speakerStyles :: [Style]
  , speakerVersion :: T.Text
  , speakerSupportedFeatures :: SupportedFeatures
  } deriving (Show, Eq)

-- Define how to parse Speaker from JSON
instance FromJSON Speaker where
  parseJSON = withObject "Speaker" $ \v -> Speaker
    <$> v .: "name"
    <*> v .: "speaker_uuid"
    <*> v .: "styles"
    <*> v .: "version"
    <*> v .: "supported_features"

-- get speaker information from API
-- baseUrl: Base URL of the API (e.g., "https://api.example.com")
getSpeakers :: T.Text -> Maybe T.Text -> IO (Either T.Text [Speaker])
getSpeakers baseUrl coreVersion = do
  -- Create API request (add query parameters if necessary)
  let endpoint = "/speakers"
      urlStr = T.unpack $ baseUrl <> endpoint
      request = case coreVersion of
        Nothing -> parseRequest_ $ "GET " <> urlStr
        Just ver -> setRequestQueryString [("core_version", Just (encodeUtf8 ver))] $ parseRequest_ $ "GET " <> urlStr
  
  -- Execute request and get response
  response <- try $ httpLBS request
  
  -- Process response
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

-- Data type representing a mora (syllable)
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

-- ToJSON instance implementation
instance ToJSON Mora where
  toJSON mora = object
    [ "text" .= moraText mora
    , "consonant" .= moraConsonant mora
    , "consonant_length" .= moraConsonantLength mora
    , "vowel" .= moraVowel mora
    , "vowel_length" .= moraVowelLength mora
    , "pitch" .= moraPitch mora
    ]

-- Data type representing an accent phrase
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

instance ToJSON AccentPhrase where
  toJSON phrase = object
    [ "moras" .= accentMoras phrase
    , "accent" .= accentAccent phrase
    , "pause_mora" .= accentPauseMora phrase
    , "is_interrogative" .= accentIsInterrogative phrase
    ]

-- Data type representing an audio query for speech synthesis
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

-- create an audio query for speech synthesis
getAudioQuery :: T.Text -> T.Text -> Int -> Maybe T.Text -> IO (Either T.Text AudioQuery)
getAudioQuery baseUrl text speakerId coreVersion = do
  -- Create API request
  let endpoint = "/audio_query"
      urlStr = T.unpack $ baseUrl <> endpoint
      baseRequest = parseRequest_ $ "POST " <> urlStr
      
      -- Set required query parameters
      reqWithParams = setRequestQueryString
        [ ("text", Just (encodeUtf8 text))
        , ("speaker", Just (encodeUtf8 $ T.pack $ show speakerId))
        ] baseRequest
      
      -- Set optional coreVersion (if specified)
      request = case coreVersion of
        Nothing -> reqWithParams
        Just ver -> setRequestQueryString [("core_version", Just (encodeUtf8 ver))] reqWithParams
  
  -- Execute request and get response
  response <- try $ httpLBS request
  
  -- Process response
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

-- perform speech synthesis
synthesisAudio :: T.Text -> AudioQuery -> Int -> Maybe Bool -> Maybe T.Text -> IO (Either T.Text BL.ByteString)
synthesisAudio baseUrl query speakerId enableInterrogativeUpspeak coreVersion = do
  -- Create API request
  let endpoint = "/synthesis"
      urlStr = T.unpack $ baseUrl <> endpoint
      baseRequest = parseRequest_ $ "POST " <> urlStr

      -- Set required query parameters
      reqWithParams = setRequestQueryString
        [ ("speaker", Just (encodeUtf8 $ T.pack $ show speakerId))
        ] baseRequest
      
      -- Set optional query parameters
      reqWithInterrogative = case enableInterrogativeUpspeak of
        Nothing -> reqWithParams
        Just enable -> setRequestQueryString [("enable_interrogative_upspeak", Just (encodeUtf8 $ T.pack $ show enable))] reqWithParams

      -- Set optional coreVersion
      reqWithCoreVersion = case coreVersion of
        Nothing -> reqWithInterrogative
        Just ver -> setRequestQueryString [("core_version", Just (encodeUtf8 ver))] reqWithInterrogative
      
      -- Set request body and headers
      request = setRequestBodyJSON query
              $ setRequestHeader "Content-Type" ["application/json"]
              $ setRequestHeader "Accept" ["audio/wav"]
                reqWithCoreVersion
  
  -- Execute request and get response
  response <- try $ httpLBS request
  
  -- Process response
  case response of
    Left e -> pure $ Left $ T.pack $ "HTTP request failed: " ++ show (e :: HttpException)
    Right res -> do
      let status = getResponseStatusCode res
      if status == 200
        then pure $ Right $ getResponseBody res
        else if status == 422
             then pure $ Left "Validation error: Invalid parameters"
             else pure $ Left $ T.pack $ "API returned status code: " ++ show status

-- get speaker style ID from speaker name and style name
findSpeakerStyle :: T.Text -> T.Text -> [Speaker] -> Maybe Int
findSpeakerStyle targetSpeakerName targetStyleName speakers = do
  speaker <- find (\s -> targetSpeakerName == speakerName s) speakers
  style <- find (\s -> targetStyleName == styleName s) (speakerStyles speaker)
  return $ styleId style

-- generate speech from text
textToSpeech :: T.Text -> T.Text -> Int -> Maybe T.Text -> IO (Either T.Text BL.ByteString)
textToSpeech baseUrl text speakerId coreVersion = do
  -- Get query for speech synthesis
  audioQueryResult <- getAudioQuery baseUrl text speakerId coreVersion
  case audioQueryResult of
    Left err -> pure $ Left err
    Right query -> synthesisAudio baseUrl query speakerId Nothing coreVersion

