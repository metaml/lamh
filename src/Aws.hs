module Aws where

import Data.Aeson
import Data.Proxy
import Data.Text
import Data.Text.Conversions
import Event.ApiGatewayInput
import Event.ApiGatewayOutput
import Event.Event
import Event.Kinesis
import Event.S3
import Event.Sns
import GHC.Generics
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Polysemy
import Servant.API
import Servant.Client

-- servant

type Api = "2018-06-01" :> "runtime" :> "invocation" :> "next" :> Get '[JSON] AwsEvent
           :<|> "2018-06-01" :> "runtime" :> "invocation" :> Capture "eventid" EventId :> "response" :> Post '[JSON] Event
           :<|> "2018-06-01" :> "runtime" :> "invocation" :> Capture "eventid" EventId :> "error" :> Post '[JSON] Error
           :<|> "2018-06-01" :> "runtime" :> "invocation" :> "init" :> "error" :> Post '[JSON] Error

api :: Proxy Api
api = Proxy

getEvent :: ClientM AwsEvent
ackEvent :: EventId -> ClientM Event
ackError :: EventId -> ClientM Error
initError :: ClientM Error

getEvent :<|> ackEvent :<|> ackError :<|> initError = client api

getEvent' :: BaseUrl -> IO (Either ClientError AwsEvent)
getEvent' url = do
  mgr <- newManager defaultManagerSettings
  runClientM getEvent (mkClientEnv mgr url)

ackEvent' :: BaseUrl -> EventId -> IO (Either ClientError Event)
ackEvent' url eid = do
  mgr <- newManager defaultManagerSettings
  runClientM (ackEvent eid) (mkClientEnv mgr url)

ackError' :: BaseUrl -> EventId -> IO (Either ClientError Error)
ackError' url eid = do
  mgr <- newManager defaultManagerSettings
  runClientM (ackError eid) (mkClientEnv mgr url)

initError' :: BaseUrl -> IO (Either ClientError Error)
initError' url = do
  mgr <- newManager defaultManagerSettings
  runClientM initError (mkClientEnv mgr url)

-- urls

runtimeApiEnv :: String
runtimeApiEnv = "AWS_LAMBDA_RUNTIME_API"

runtimePath :: String
runtimePath = "/2018-06-01/runtime"

type Hostname = String

getEventUrl :: Hostname -> BaseUrl
getEventUrl h = BaseUrl Http h 80 (runtimePath <>  "/invocation/next")

ackEventUrl :: Hostname -> EventId -> BaseUrl
ackEventUrl h id' = BaseUrl Http h 80 (runtimePath <> "/invocation/" <> eventId' id' <> "/response")

ackErrorUrl :: Hostname -> EventId -> BaseUrl
ackErrorUrl h id' = BaseUrl Http h 80 (runtimePath <> "/invocation/" <> eventId' id' <> "/error")

initErrorUrl :: Hostname -> BaseUrl
initErrorUrl h = BaseUrl Http h 80 (runtimePath <> "/init/error")

eventId' :: EventId -> String
eventId' = unpack . toText
