module Model.Lambda where

import Data.CaseInsensitive
import Data.HashMap.Strict
import Data.Text
import Event.Event
import Event.S3
import Model.Log
import Network.HTTP.Client
import Polysemy
import Polysemy.Reader
import Servant.Client
import qualified Network.Aws as Aws

data Lambda m a where
  GetS3Event :: Lambda m (Either ClientError S3Event)
  GetS3EventPair :: Lambda m (Either (ClientError, HashMap (CI Text) Text)  (S3Event, HashMap (CI Text) Text))
  AckEvent :: EventId -> Lambda m (Either ClientError Success)
  AckError :: EventId -> Lambda m (Either ClientError Success)
  InitError :: Error -> Lambda m (Either ClientError Success)

makeSem ''Lambda

runLambdaIO :: Members '[Reader BaseUrl, Reader Manager, Log, Embed IO] r => Sem (Lambda ': r) a -> Sem r a
runLambdaIO = interpret $ \case
  GetS3Event -> do
    logStderr "- GetS3Event"
    url <- ask @BaseUrl
    mgr <- ask @Manager
    embed $ Aws.getS3Event' mgr url
  GetS3EventPair -> do
    logStderr "- GetS3EventPair"
    url <- ask @BaseUrl
    mgr <- ask @Manager
    embed $ Aws.getS3EventPair mgr url
  AckEvent eid -> do
    logStderr $ "- AckEvent"
    url <- ask @BaseUrl
    mgr <- ask @Manager
    embed $ Aws.ackEvent' mgr url eid
  AckError eid -> do
    logStderr $ "- AckError"
    url <- ask @BaseUrl
    mgr <- ask @Manager
    embed $ Aws.ackError' mgr url eid
  InitError err -> do
    logStderr $ "- InitError"
    url <- ask @BaseUrl
    mgr <- ask @Manager
    embed $ Aws.initError' mgr url
