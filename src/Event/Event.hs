{-# language StandaloneDeriving #-}
module Event.Event where

import Control.Lens
import Data.Aeson
import Data.Aeson.Text
import Data.String (IsString)
import Data.Text hiding (drop)
import Data.Text.Conversions
import Event.ApiGatewayInput
import Event.ApiGatewayOutput
import Event.S3
import Event.Sns
import GHC.Generics

newtype EventId = EventId Text
  deriving (Eq, FromJSON, ToJSON, Generic, Show)
  deriving newtype (IsString, ToText)

data AwsEvent a = AwsEvent { _eventId :: EventId
                           , _eventBody :: Either Error Event
                           } deriving (Eq, Generic, Show)

instance (ToJSON a) => ToJSON (AwsEvent a) where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = rekey }

instance (FromJSON a) => FromJSON (AwsEvent a) where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = rekey }

rekey :: String -> String
rekey "_eventId" = "eventID"
rekey k = drop 1 k

data Event = ApiGatewayInput | ApiGatewayOutput | S3 | Sns
  deriving (Eq, FromJSON, ToJSON, Generic, Show)

data Error = Error { _errorType :: Text
                   , _errorMessage :: Text
                   } deriving (Eq, Generic, Show)

instance ToJSON Error where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON Error where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

makeLenses ''AwsEvent
makeLenses ''Error
