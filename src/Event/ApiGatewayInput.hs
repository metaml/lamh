module Event.ApiGatewayInput ( ApiGatewayInput
                             ) where
import Control.Lens
import Data.Aeson
import Data.HashMap.Strict
import Data.Text (Text)
import GHC.Generics

data ApiGatewayInput = ApiGatewayInput { _resource :: Text
                                       , _path :: Text
                                       , _httpMethod :: Text
                                       , _headers :: Maybe (HashMap Text Text)
                                       , _multiValueHeaders :: Maybe (HashMap Text [Text])
                                       , _queryStringParameters :: Maybe (HashMap Text Text)
                                       , _multiValueQueryStringParameters :: Maybe (HashMap Text [Text])
                                       , _pathParameters :: Maybe (HashMap Text Text)
                                       , _stageVariables :: Maybe (HashMap Text Text)
                                       , _requestContext :: HashMap Text Value
                                       , _body :: Text
                                       , _isBase64Encoded :: Bool
                                       } deriving (Eq, Generic, Show)

instance ToJSON ApiGatewayInput where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON ApiGatewayInput where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

makeLenses ''ApiGatewayInput
