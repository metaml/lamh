{-# language AllowAmbiguousTypes #-}
{-# language NoMonomorphismRestriction #-}
module Event.ApiGatewayOutput where

import Data.Aeson
import Data.HashMap.Strict
import Data.Text (Text)
import GHC.Generics

data ApiGatewayOutput = ApiGatewayOutput { isBase64Encoded :: Bool
                                         , statusCode :: Int
                                         , headers :: Maybe (HashMap Text Text)
                                         , multiValueHeaders :: Maybe (HashMap Text [Text])
                                         , body :: Text
                                         } deriving (Eq, FromJSON, ToJSON, Generic, Show)

apiGatewayOutput :: Text -> ApiGatewayOutput
apiGatewayOutput = ApiGatewayOutput False 200 Nothing Nothing
