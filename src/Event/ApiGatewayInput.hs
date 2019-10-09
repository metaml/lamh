{-# language AllowAmbiguousTypes #-}
{-# language NoMonomorphismRestriction #-}
module Event.ApiGatewayInput where

import Data.Aeson
import Data.HashMap.Strict
import Data.Text (Text)
import GHC.Generics

data ApiGatewayInput = ApiGatewayInput { resource :: Text
                                       , path :: Text
                                       , httpMethod :: Text
                                       , headers :: Maybe (HashMap Text Text)
                                       , multiValueHeaders :: Maybe (HashMap Text [Text])
                                       , queryStringParameters :: Maybe (HashMap Text Text)
                                       , multiValueQueryStringParameters :: Maybe (HashMap Text [Text])
                                       , pathParameters :: Maybe (HashMap Text Text)
                                       , stageVariables :: Maybe (HashMap Text Text)
                                       , requestContext :: HashMap Text Value
                                       , body :: Text
                                       , isBase64Encoded :: Bool
                                       } deriving (Eq, FromJSON, ToJSON, Generic, Show)
