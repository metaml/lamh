module Network.Aws where

import Data.Proxy
import Event.Event
import Network.HTTP.Client (Manager)
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

getEvent' :: Manager -> BaseUrl -> IO (Either ClientError AwsEvent)
getEvent' mgr url = runClientM getEvent (mkClientEnv mgr url)

ackEvent' :: Manager -> BaseUrl -> EventId -> IO (Either ClientError Event)
ackEvent' mgr url eid = runClientM (ackEvent eid) (mkClientEnv mgr url)

ackError' :: Manager -> BaseUrl -> EventId -> IO (Either ClientError Error)
ackError' mgr url eid = runClientM (ackError eid) (mkClientEnv mgr url)

initError' :: Manager -> BaseUrl -> IO (Either ClientError Error)
initError' mgr url = runClientM initError (mkClientEnv mgr url)
