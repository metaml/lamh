module Event.Sns where

import Control.Lens
import Data.Aeson
import Data.HashMap.Strict
import Data.Text hiding (drop)
import GHC.Generics
import qualified Data.Char as C

data MessageAttribute = MessageAttribute { _attributeType :: Text
                                         , _value :: Text
                                         } deriving (Eq, Generic, Show)

instance ToJSON MessageAttribute where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = rekey }

instance FromJSON MessageAttribute where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = rekey }

data Sns = Sns { _signatureVersion :: Text
               , _timestamp :: Text
               , _signature :: Text
               , _signingCertUrl :: Text
               , _messageId :: Text
               , _message :: Text
               , _messageAttributes :: Maybe (HashMap Text MessageAttribute)
               , _snsType :: Text
               , _unsubscribeUrl :: Text
               , _topicArn :: Text
               , _subject :: Maybe Text
               } deriving (Eq, Generic, Show)

instance ToJSON Sns where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = rekey }

instance FromJSON Sns where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = rekey}

data Record = Record { _eventVersion :: Text
                     , _eventSource :: Text
                     , _eventSubscriptionArn :: Text
                     , _sns :: Sns
                     } deriving (Eq, Generic, Show)

instance ToJSON Record where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = rekey }

instance FromJSON Record where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = rekey }

newtype SnsEvent = SnsEvent { _records :: [Record] }
  deriving (Eq, Generic, Show)

instance FromJSON SnsEvent where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = rekey }

instance ToJSON SnsEvent where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = rekey }

rekey :: String -> String
rekey "_snsType" = "Type"
rekey "_attributeType" = "Type"
rekey v  = capitalize . drop 1 $ v

capitalize :: String -> String
capitalize "" = ""
capitalize (x:xs) = C.toUpper x : xs

makeLenses ''MessageAttribute
makeLenses ''Sns
makeLenses ''Record
makeLenses ''SnsEvent
