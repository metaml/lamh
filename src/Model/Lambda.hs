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
import Util
import qualified Network.Aws as Aws

data Lambda m a where
  GetS3Event :: Lambda m (Either ClientError S3Event)
  GetS3EventPair :: Lambda m (Either (ClientError, HashMap (CI Text) Text)  (S3Event, HashMap (CI Text) Text))
  AckEvent :: EventId -> Lambda m (Either ClientError Success)
  AckError :: EventId -> Lambda m (Either ClientError Success)
  InitError :: Error -> Lambda m (Either ClientError Success)

makeSem ''Lambda

lambdaToIO :: Members '[Reader BaseUrl, Reader Manager, Log, Embed IO] r => Sem (Lambda ': r) a -> Sem r a
lambdaToIO = interpret $ \case
  GetS3Event -> do
    url <- ask @BaseUrl
    mgr <- ask @Manager
    logStderr $ "- GetS3Event: url=" <> showt url
    embed $ Aws.getS3Event' mgr url
  GetS3EventPair -> do
    url <- ask @BaseUrl
    mgr <- ask @Manager
    logStderr $ "- GetS3EventPair: url=" <> showt url
    embed $ Aws.getS3EventPair mgr url
  AckEvent eid -> do
    url <- ask @BaseUrl
    mgr <- ask @Manager
    logStderr $ "- AckEvent: url=" <> showt url
    embed $ Aws.ackEvent' mgr url eid
  AckError eid -> do
    url <- ask @BaseUrl
    mgr <- ask @Manager
    logStderr $ "- AckError: eid=" <> showt eid
    embed $ Aws.ackError' mgr url eid
  InitError err -> do
    logStderr $ "- InitError: err=" <> showt err
    url <- ask @BaseUrl
    mgr <- ask @Manager
    embed $ Aws.initError' mgr url
