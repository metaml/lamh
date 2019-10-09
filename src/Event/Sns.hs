{-# language AllowAmbiguousTypes #-}
{-# language NoMonomorphismRestriction #-}
module Event.Sns where

import Data.Aeson
import Data.Char as C
import Data.HashMap.Strict
import Data.Text hiding (drop)
import GHC.Generics

data MessageAttribute = MessageAttribute { attributeType :: Text
                                         , value :: Text
                                         } deriving (Eq, Generic, Show)

instance ToJSON MessageAttribute where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = rekey }

instance FromJSON MessageAttribute where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = rekey }

data Sns = Sns { signatureVersion :: Text
               , timestamp :: Text
               , signature :: Text
               , signingCertUrl :: Text
               , messageId :: Text
               , message :: Text
               , messageAttributes :: Maybe (HashMap Text MessageAttribute)
               , snsType :: Text
               , unsubscribeUrl :: Text
               , topicArn :: Text
               , subject :: Maybe Text
               } deriving (Eq, Generic, Show)

instance ToJSON Sns where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = rekey }

instance FromJSON Sns where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = rekey}

data Record = Record { eventVersion :: Text
                     , eventSource :: Text
                     , eventSubscriptionArn :: Text
                     , sns :: Sns
                     } deriving (Eq, Generic, Show)

instance ToJSON Record where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = rekey }

instance FromJSON Record where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = rekey }

newtype SnsEvent = SnsEvent { records :: [Record] }
  deriving (Eq, Generic, Show)

instance FromJSON SnsEvent where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = rekey }

instance ToJSON SnsEvent where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = rekey }

rekey :: String -> String
rekey "snsType" = "Type"
rekey "attributeType" = "Type"
rekey v  = capitalize . drop 1 $ v

capitalize :: String -> String
capitalize "" = ""
capitalize (x:xs) = C.toUpper x : xs
