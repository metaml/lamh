module Model.Eg.EchoEvent where

import Prelude hiding (log, lookup)
import Control.Monad
import Control.Exception hiding (catch, throw)
import Data.CaseInsensitive (CI, mk)
import Data.Generics.Internal.VL.Lens
import Data.Generics.Product.Fields
import Data.HashMap.Strict (lookup)
import Data.List.Split
import Data.Text hiding (splitOn)
import Event.Event as Event
import Model.Env
import Model.Lambda
import Model.Log
import Network.HTTP.Client
import Polysemy
import Polysemy.Error as P
import Polysemy.Reader
import Servant.Client (BaseUrl(..), Scheme(..))
import Servant.Client.Core.ClientError
import Util
import qualified System.Environment as E

run :: IO ()
run = do
  manager <- newManager defaultManagerSettings
  hostport <- E.getEnv "AWS_LAMBDA_RUNTIME_API"
  let ps = splitOn ":" hostport
      (hostname, port') = (ps !! 0, read (ps !! 1) :: Int)
      baseUrl = BaseUrl Http hostname port' ""
  forever $
    runM . logToIO . runReader manager . runReader baseUrl $ echoEventIO

echoEventIO :: Sem '[Reader BaseUrl, Reader Manager, Log, Embed IO] ()
echoEventIO = do
  r <- (runError @SomeException) . envToIO . lambdaToIO $ echoEvent
  case r of
    Left x -> logStderr $ "- failure: " <> showt x
    Right _ -> logStderr "- success"

echoEvent :: Members '[P.Error SomeException, Log, Env, Lambda] r => Sem r ()
echoEvent = catch @SomeException echo err
  where
    echo = do
      getS3EventPair >>= \case
        Left err' -> do
          logStderr $ "Left getS3EventPair r==" <> showt err'
          case lookup lambdaRuntimeTraceId (err' ^. field @"header") of
            Just tid -> setEnv "_X_AMZN_TRACE_ID" (show tid)
            Nothing -> logStderr $ "- (Left) no "
                                   <> showt lambdaRuntimeTraceId
                                   <> ": "
                                   <> showt err'
          case lookup lambdaRuntimeAwsRequestId (err' ^. field @"header") of
            Just reqId -> ackError (EventId reqId) (toError $ err' ^. field @"error")
                          >>= \r' -> logStderr $ "ackError response=" <> showt r'
            Nothing -> do
              logStderr $ "- AckEvent-- no "
                          <> showt lambdaRuntimeAwsRequestId
                          <> ": "
                          <> showt err'
              initError (toError $ err' ^. field @"error") >>= \r' -> logStderr $ "ackEvent response=" <> showt r'
        Right evt -> do
          logStderr $ "Right getS3EventPair r=" <> showt evt
          case lookup lambdaRuntimeTraceId (evt ^. field @"header") of
            Just tid -> setEnv "_X_AMZN_TRACE_ID" (show tid)
            Nothing -> logStderr $ "- (Right) no "
                                   <> showt lambdaRuntimeTraceId
                                   <> ": "
                                   <> showt evt
          case lookup lambdaRuntimeAwsRequestId (evt ^. field @"header") of
            Just reqId -> ackEvent (EventId reqId)
                          >>= \r' -> logStderr $ "ackEvent response=" <> showt r'
            Nothing -> logStderr $ "- AckEvent-- no "
                                   <> showt lambdaRuntimeAwsRequestId
                                   <> ": "
                                   <> showt evt
          logStderr $ "Right getS3EventPair r=" <> showt evt
    err = \e -> initError (Error "Exception" (showt e)) >>= \r -> logStderr $ "initError response=" <> showt r

lambdaRuntimeAwsRequestId :: CI Text
lambdaRuntimeAwsRequestId = mk "Lambda-Runtime-Aws-Request-Id"

lambdaRuntimeTraceId :: CI Text
lambdaRuntimeTraceId = mk "Lambda-Runtime-Trace-Id"

toError :: ClientError -> Event.Error
toError r@(FailureResponse _ _) = Error "FailureResponse" (showt r)
toError r@(DecodeFailure _ _) = Error "DecodeFailure" (showt r)
toError r@(UnsupportedContentType _ _) = Error "UnsupportedContentType" (showt r)
toError r@(InvalidContentTypeHeader _) = Error "InvalidContentTypeHeader" (showt r)
toError r@(ConnectionError _) = Error "ConnectionError" (showt r)
