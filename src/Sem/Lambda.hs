module Sem.Lambda where

import Event.Event
import Network.HTTP.Client
import Polysemy
import Polysemy.Reader
import Servant.Client
import qualified Network.Aws as Aws

data Lambda m a where
  GetEvent :: Lambda m (Either ClientError AwsEvent)
  AckEvent :: EventId -> Lambda m (Either ClientError Event)
  AckError :: EventId -> Lambda m (Either ClientError Error)
  InitError :: Error -> Lambda m (Either ClientError Error)

makeSem ''Lambda

runLambdaIO :: Members '[Reader BaseUrl, Reader Manager, Lift IO] r => Sem (Lambda ': r) a -> Sem r a
runLambdaIO = interpret $ \case
  GetEvent -> do
    url <- ask @BaseUrl
    mgr <- ask @Manager
    sendM $ Aws.getEvent' mgr url
  AckEvent eid -> do
    url <- ask @BaseUrl
    mgr <- ask @Manager
    sendM $ Aws.ackEvent' mgr url eid
  AckError eid -> do
    url <- ask @BaseUrl
    mgr <- ask @Manager
    sendM $ Aws.ackError' mgr url eid
  InitError err -> do
    url <- ask @BaseUrl
    mgr <- ask @Manager
    sendM $ Aws.initError' mgr url
