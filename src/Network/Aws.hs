module Network.Aws where

import Control.Monad.Free
import Data.Foldable (toList)
import Data.Proxy
import Data.Text.Encoding (decodeUtf8)
import Event.Event
import Event.S3
import Network.HTTP.Client (Manager)
import Servant.API
import Servant.Client
import qualified Network.HTTP.Client as HTTP
import qualified Servant.Client.Free as F
import qualified Servant.Client.Internal.HttpClient as I

type Api = "2018-06-01" :> "runtime" :> "invocation" :> "next" :> Get '[JSON] S3Event
           :<|> "2018-06-01" :> "runtime" :> "invocation" :> Capture "eventid" EventId :> "response" :> Post '[JSON] Status
           :<|> "2018-06-01" :> "runtime" :> "invocation" :> Capture "eventid" EventId :> "error" :> Post '[JSON] Error
           :<|> "2018-06-01" :> "runtime" :> "invocation" :> "init" :> "error" :> Post '[JSON] Error
api :: Proxy Api
api = Proxy

getS3Event :: ClientM S3Event
ackEvent :: EventId -> ClientM Status
ackError :: EventId -> ClientM Error
initError :: ClientM Error

getS3Event :<|> ackEvent :<|> ackError :<|> initError = client api

getS3Event' :: Manager -> BaseUrl -> IO (Either ClientError S3Event)
getS3Event' mgr url = runClientM getS3Event (mkClientEnv mgr url)

ackEvent' :: Manager -> BaseUrl -> EventId -> IO (Either ClientError Status)
ackEvent' mgr url eid = runClientM (ackEvent eid) (mkClientEnv mgr url)

ackError' :: Manager -> BaseUrl -> EventId -> IO (Either ClientError Error)
ackError' mgr url eid = runClientM (ackError eid) (mkClientEnv mgr url)

initError' :: Manager -> BaseUrl -> IO (Either ClientError Error)
initError' mgr url = runClientM initError (mkClientEnv mgr url)

type Api' = "2018-06-01" :> "runtime" :> "invocation" :> "next" :> Get '[JSON] S3Event
api' :: Proxy Api'
api' = Proxy

getFS3Event :: Free F.ClientF S3Event
getFS3Event = F.client api'

-- NB: there must be an easier way
getS3EventPair :: Manager -> BaseUrl -> IO (Either (ClientError, Maybe EventId) (S3Event, EventId))
getS3EventPair mgr url = case getFS3Event of
  Pure r -> error $ show r
  Free (F.Throw err) -> return $ Left (err, Nothing)
  Free (F.RunRequest req k) -> do
    let req' = I.requestToClientRequest url req
    res' <- HTTP.httpLbs req' mgr
    let res = I.clientResponseToResponse id res'
        evtId = EventId . decodeUtf8 <$> lookup "Lambda-Runtime-Aws-Request-Id" (toList $ responseHeaders res)
        evt = k res
    case (evtId, evt) of
      (Just evId, Pure ev) -> return $ Right (ev, evId)
      (Nothing, Pure ev) -> error $ "shoud not happen | " <> show ev
      (evId, Free (F.Throw err)) -> return $ Left (err, evId)
      (_, Free (F.RunRequest _ _)) -> error "should not happen"

-- https://docs.aws.amazon.com/lambda/latest/dg/runtimes-api.html#runtimes-api-initerror

--REQUEST_ID=156cb537-e2d4-11e8-9b34-d36013741fb9
-- curl -X POST  "http://${AWS_LAMBDA_RUNTIME_API}/2018-06-01/runtime/invocation/$REQUEST_ID/response"  -d "SUCCESS"

-- REQUEST_ID=156cb537-e2d4-11e8-9b34-d36013741fb9
-- ERROR="{\"errorMessage\" : \"Error parsing event data.\", \"errorType\" : \"InvalidEventDataException\"}"
-- curl -X POST "http://${AWS_LAMBDA_RUNTIME_API}/2018-06-01/runtime/invocation/$REQUEST_ID/error" -d "$ERROR" --header "Lambda-Runtime-Function-Error-Type: Unhandled"
