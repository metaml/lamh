{-# language AllowAmbiguousTypes #-}
{-# language NoMonomorphismRestriction #-}
module Event.Event where

import Data.Aeson hiding (Error)
import Data.String (IsString)
import Data.Text hiding (drop)
import Data.Text.Conversions
import GHC.Generics
import Servant.API (ToHttpApiData(..))

newtype EventId = EventId Text
  deriving (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)
  deriving newtype (IsString, ToText)

instance ToHttpApiData EventId where
  toUrlPiece = toText

data Error = Error { errorType :: Text
                   , errorMessage :: Text
                   } deriving (Eq, FromJSON, ToJSON, Generic, Show)

data Success = Success { status :: Text
                       } deriving (Eq, FromJSON, ToJSON, Generic, Show)

data Event = ApiGatewayInput | ApiGatewayOutput | S3 | Sns
  deriving (Eq, FromJSON, ToJSON, Generic, Show)

data AwsEvent = AwsEvent { eventId :: EventId
                         , eventBody :: Either Error Event
                         } deriving (Eq, Generic, Show)

instance ToJSON AwsEvent where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = rekey }

instance FromJSON AwsEvent where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = rekey }

data Records = Records [Event]
  deriving (Eq, FromJSON, ToJSON, Generic, Show)

rekey :: String -> String
rekey "eventId" = "eventID"
rekey k = k
