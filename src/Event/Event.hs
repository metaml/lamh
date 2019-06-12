module Event.Event where

import Control.Lens
import Data.Aeson hiding (Error)
import Data.String (IsString)
import Data.Text hiding (drop)
import Data.Text.Conversions
import GHC.Generics
import Internal.EventUtil (rekey)
import Servant.API (ToHttpApiData(..))

newtype EventId = EventId Text
  deriving (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)
  deriving newtype (IsString, ToText)

instance ToHttpApiData EventId where
  toUrlPiece = toText

data Error = Error { _errorType :: Text
                   , _errorMessage :: Text
                   } deriving (Eq, Generic, Show)

instance ToJSON Error where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON Error where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

data Success = Success { _status :: Text
                       } deriving (Eq, Generic, Show)

instance ToJSON Success where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON Success where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

data Event = ApiGatewayInput | ApiGatewayOutput | S3 | Sns
  deriving (Eq, FromJSON, ToJSON, Generic, Show)

data AwsEvent = AwsEvent { _eventId :: EventId
                         , _eventBody :: Either Error Event
                         } deriving (Eq, Generic, Show)
instance ToJSON AwsEvent where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = rekey }

instance FromJSON AwsEvent where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = rekey }

data Records = Records [Event]
  deriving (Eq, FromJSON, ToJSON, Generic, Show)

makeLenses ''Event
makeLenses ''AwsEvent
makeLenses ''Error
makeLenses ''Success
