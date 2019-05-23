module Event.ApiGatewayOutput where
import Control.Lens
import Data.Aeson
import Data.HashMap.Strict
import Data.Text (Text)
import GHC.Generics

data ApiGatewayOutput = ApiGatewayOutput { _isBase64Encoded :: Bool
                                         , _statusCode :: Int
                                         , _headers :: Maybe (HashMap Text Text)
                                         , _multiValueHeaders :: Maybe (HashMap Text [Text])
                                         , _body :: Text
                                         } deriving (Eq, Generic, Show)

instance ToJSON ApiGatewayOutput where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON ApiGatewayOutput where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

makeLenses ''ApiGatewayOutput

apiGatewayOutput :: Text -> ApiGatewayOutput
apiGatewayOutput = ApiGatewayOutput False 200 Nothing Nothing
