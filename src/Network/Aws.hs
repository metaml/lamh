module Network.Aws where

import Data.CaseInsensitive hiding (map)
import Control.Exception
import Control.Monad.Free
import Data.CaseInsensitive as CI hiding (map)
import Data.Foldable (toList)
import Data.Proxy
import Data.Sequence
import Data.Text.Encoding (decodeUtf8)
import Event.Event
import Event.S3
import Model.Event
import Network.HTTP.Client (HttpException(..), Manager)
import Network.HTTP.Types (Header)
import Servant.API hiding (Header)
import Servant.Client
import qualified Data.HashMap.Strict as H
import qualified Network.HTTP.Client as HTTP
import qualified Servant.Client.Free as F
import qualified Servant.Client.Internal.HttpClient as I

type Api = "2018-06-01" :> "runtime" :> "invocation" :> "next" :> Get '[JSON] S3Event
           :<|> "2018-06-01" :> "runtime" :> "invocation" :> Capture "eventid" EventId :> "response" :> Post '[JSON] Success
           :<|> "2018-06-01" :> "runtime" :> "invocation" :> Capture "eventid" EventId :> "error" :> ReqBody '[JSON] Error :> Post '[JSON] Success
           :<|> "2018-06-01" :> "runtime" :> "invocation" :> "init" :> "error" :> Post '[JSON] Success

api :: Proxy Api
api = Proxy

getS3Event :: ClientM S3Event
ackEvent :: EventId -> ClientM Success
ackError :: EventId -> Error -> ClientM Success
initError :: ClientM Success

getS3Event :<|> ackEvent :<|> ackError :<|> initError = client api

getS3Event' :: Manager -> BaseUrl -> IO (Either ClientError S3Event)
getS3Event' mgr url = runClientM getS3Event (mkClientEnv mgr url)

ackEvent' :: Manager -> BaseUrl -> EventId -> IO (Either ClientError Success)
ackEvent' mgr url eid = runClientM (ackEvent eid) (mkClientEnv mgr url)

ackError' :: Manager -> BaseUrl -> EventId -> Error -> IO (Either ClientError Success)
ackError' mgr url eid err = runClientM (ackError eid err) (mkClientEnv mgr url)

initError' :: Manager -> BaseUrl -> IO (Either ClientError Success)
initError' mgr url = runClientM initError (mkClientEnv mgr url)

type Api' = "2018-06-01" :> "runtime" :> "invocation" :> "next" :> Get '[JSON] S3Event
api' :: Proxy Api'
api' = Proxy

getFS3Event :: Free F.ClientF S3Event
getFS3Event = F.client api'

getS3EventPair :: Manager -> BaseUrl -> IO (Either ClientError' S3Event')
getS3EventPair mgr url = getS3EventPair' mgr url >>= \case
  Left (err, hs) -> pure $ Left $ ClientError' err (hmap hs)
  Right (evt, hs) -> pure $ Right $ S3Event' evt (hmap hs)
  where
    hmap hs = H.fromList $ map (\(x, y) -> (CI.mk . decodeUtf8 $ original x, decodeUtf8 y)) $ toList hs

-- NB: there must be an easier way
getS3EventPair' :: Manager -> BaseUrl -> IO (Either (ClientError, Seq Header) (S3Event, Seq Header))
getS3EventPair' mgr url = case getFS3Event of
  Pure evt -> pure $ Right (evt, Empty) -- does this make sense?
  Free (F.Throw err) -> pure $ Left (err, Empty)
  Free (F.RunRequest req k) -> do
    let req' = I.defaultMakeClientRequest url req
    try (HTTP.httpLbs req' mgr) >>= \case
      Left (e :: HttpException) -> pure $ Left (ConnectionError (SomeException e), Empty)
      Right res' -> do
        let res = I.clientResponseToResponse id res'
            hdrs = responseHeaders res
        case k res of
          Pure evt -> pure $ Right (evt, hdrs)
          Free (F.Throw e) -> pure $ Left (e, hdrs)
          Free (F.RunRequest _ _) -> pure $ Left (ConnectionError (SomeException FreeException), Empty)

data CustomException = FreeException | PureException
  deriving (Eq, Exception, Show)

-- https://docs.aws.amazon.com/lambda/latest/dg/runtimes-api.html#runtimes-api-initerror
--
-- REQUEST_ID=156cb537-e2d4-11e8-9b34-d36013741fb9
-- curl -X POST  "http://${AWS_LAMBDA_RUNTIME_API}/2018-06-01/runtime/invocation/$REQUEST_ID/response"  -d "SUCCESS"
--
-- REQUEST_ID=156cb537-e2d4-11e8-9b34-d36013741fb9
-- ERROR="{\"errorMessage\" : \"Error parsing event data.\", \"errorType\" : \"InvalidEventDataException\"}"
-- curl -X POST "http://${AWS_LAMBDA_RUNTIME_API}/2018-06-01/runtime/invocation/$REQUEST_ID/error" -d "$ERROR" --header "Lambda-Runtime-Function-Error-Type: Unhandled"
