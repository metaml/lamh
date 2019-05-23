module Sem.Lambda where

import Event.Event
import Network.HTTP.Client (Manager)
import Polysemy
import Polysemy.Reader
import Servant.Client
import qualified Network.Aws as Aws

type Hostname = String

data Lambda m a where
  GetEvent :: Lambda m (Either ClientError AwsEvent)
  AckEvent :: EventId -> Lambda m (Either ClientError Event)
  AckError :: EventId -> Lambda m (Either ClientError Error)
  InitError :: Error -> Lambda m (Either ClientError Error)

makeSem ''Lambda

getEvent' :: Member Lambda r => Sem r (Either ClientError AwsEvent)
getEvent' = getEvent >>= return

ackEvent' :: Member Lambda r => EventId -> Sem r ()
ackEvent' eid = ackEvent eid >> return ()

ackError' :: Member Lambda r => EventId -> Sem r ()
ackError' eid = ackError eid >> return ()

initError' :: Member Lambda r => Error -> Sem r ()
initError' err = initError err >> return ()

runLambdaIO :: Members '[Reader Hostname, Reader Manager, Lift IO] r => Sem (Lambda ': r) a -> Sem r a
runLambdaIO = interpret $ \case
  GetEvent -> do
    h <- ask @Hostname
    mgr <- ask @Manager
    sendM $ Aws.getEvent' mgr (Aws.getEventUrl h)
  AckEvent eid -> do
    h <- ask @Hostname
    mgr <- ask @Manager
    sendM $ Aws.ackEvent' mgr (Aws.ackEventUrl h eid) eid
  AckError eid -> do
    h <- ask @Hostname
    mgr <- ask @Manager
    sendM $ Aws.ackError' mgr (Aws.ackErrorUrl h eid) eid
  InitError err -> do
    h <- ask @Hostname
    mgr <- ask @Manager
    sendM $ Aws.initError' mgr (Aws.initErrorUrl h)
