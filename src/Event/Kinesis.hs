module Event.Kinesis ( Kinesis
                     , KinesisEvent
                     , Record
                     ) where
import Control.Lens
import Data.Aeson
import Data.Text (Text)
import GHC.Generics

data Kinesis = Kinesis { _partitionKey :: Text
                       , _data' :: Text
                       , _kinesisSchemaVersion :: Text
                       , _sequenceNumber :: Text
                       } deriving (Eq, Generic, Show)

instance ToJSON Kinesis where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = rekey }

instance FromJSON Kinesis where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = rekey }

rekey :: String -> String
rekey "_data'" = "data"
rekey k = drop 1 k

data Record = Record { _eventId :: Text
                     , _eventVersion :: Text
                     , _kinesis :: Kinesis
                     , _invokeIdentityArn :: Text
                     , _eventName :: Text
                     , _eventSourceARN :: Text
                     , _eventSource :: Text
                     , _awsRegion :: Text
                     } deriving (Eq, Generic, Show)

instance ToJSON Record where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = rekey'' }

instance FromJSON Record where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = rekey'' }

rekey'' :: String -> String
rekey'' "_eventId" = "eventID"
rekey'' k          = drop 1 k

newtype KinesisEvent = KinesisEvent { _records :: [Record] }
  deriving (Eq, Generic, Show)

instance ToJSON KinesisEvent where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = rekey' }

instance FromJSON KinesisEvent where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = rekey' }

rekey' :: String -> String
rekey' "_records" = "Records"
rekey' k          = drop 1 k

makeLenses ''Kinesis
makeLenses ''Record
makeLenses ''KinesisEvent
