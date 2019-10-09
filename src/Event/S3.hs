{-# language AllowAmbiguousTypes #-}
{-# language NoMonomorphismRestriction #-}
module Event.S3 where

import Data.Aeson
import Data.HashMap.Strict
import Data.Text hiding (drop)
import GHC.Generics

data S3Object = S3Object { key :: Text
                         , size :: Maybe Integer
                         , eTag :: Maybe Text
                         , versionId :: Maybe Text
                         , sequencer :: Text
                         } deriving (Eq, FromJSON, ToJSON, Generic, Show)

data Bucket = Bucket { name :: Text
                     , arn :: Text
                     , ownerIdentity :: Maybe (HashMap Text Text)
                     } deriving (Eq, FromJSON, ToJSON, Generic, Show)

data S3 = S3 { s3SchemaVersion :: Text
             , configurationId :: Text
             , bucket :: Bucket
             , s3object :: S3Object
             } deriving (Eq, Generic, Show)

instance ToJSON S3 where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = rekey }

instance FromJSON S3 where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = rekey }

data Record = Record { eventVersion :: Text
                     , eventSource :: Text
                     , awsRegion :: Text
                     , eventTime :: Text
                     , eventName :: Text
                     , s3 :: S3
                     , userIdentity :: Maybe (HashMap Text Text)
                     , requestParameters :: Maybe (HashMap Text Text)
                     , responseElements :: Maybe (HashMap Text Text)
                     , glacierEventData :: Maybe (HashMap Text Value)
                     } deriving (Eq, FromJSON, ToJSON, Generic, Show)

newtype S3Event = S3Event { records :: [Record] }
  deriving (Eq, Generic, Show)

instance ToJSON S3Event where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = rekey' }

instance FromJSON S3Event where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = rekey' }

rekey :: String -> String
rekey "s3object" = "object"
rekey k = k

rekey' :: String -> String
rekey' "records" = "Records"
rekey' k = k
