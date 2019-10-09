{-# language AllowAmbiguousTypes #-}
{-# language NoMonomorphismRestriction #-}
module Event.Kinesis where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics

data Kinesis = Kinesis { partitionKey :: Text
                       , data' :: Text
                       , kinesisSchemaVersion :: Text
                       , sequenceNumber :: Text
                       } deriving (Eq, Generic, Show)

instance ToJSON Kinesis where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = rekey }

instance FromJSON Kinesis where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = rekey }

data Record = Record { eventId :: Text
                     , eventVersion :: Text
                     , kinesis :: Kinesis
                     , invokeIdentityArn :: Text
                     , eventName :: Text
                     , eventSourceARN :: Text
                     , eventSource :: Text
                     , awsRegion :: Text
                     } deriving (Eq, Generic, Show)

instance ToJSON Record where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = rekey'' }

instance FromJSON Record where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = rekey'' }

newtype KinesisEvent = KinesisEvent { records :: [Record] }
  deriving (Eq, Generic, Show)

instance ToJSON KinesisEvent where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = rekey' }

instance FromJSON KinesisEvent where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = rekey' }

rekey :: String -> String
rekey "data'" = "data"
rekey k = k

rekey' :: String -> String
rekey' "records" = "Records"
rekey' k = k

rekey'' :: String -> String
rekey'' "eventId" = "eventID"
rekey'' k = k
