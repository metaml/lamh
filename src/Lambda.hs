module Lambda where

import Prelude hiding (log, lookup)
import Control.Monad
import Control.Exception hiding (catch, throw)
import Data.CaseInsensitive (CI, mk)
import Data.HashMap.Strict (lookup)
import Data.List.Split
import Data.Text hiding (splitOn)
import Event.Event hiding (Error)
import Model.Env
import Model.Lambda
import Model.Log
import Network.HTTP.Client
import Polysemy
import Polysemy.Error
import Polysemy.Reader
import Servant.Client (BaseUrl(..), Scheme(..))
import System.Environment
import Util

run :: IO ()
run = do
  manager <- newManager defaultManagerSettings
  hostport <- getEnv "AWS_LAMBDA_RUNTIME_API"
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
  return ()

echoEvent :: Members '[Error SomeException, Log, Env, Lambda] r => Sem r ()
echoEvent = catch @SomeException
              do getS3EventPair >>= \case
                   Left (err, hmap) -> do
                     let traceId = lookup lambdaRuntimeTraceId hmap
                     case traceId of
                       Just tid -> set "_X_AMZN_TRACE_ID" (show tid)
                       Nothing -> logStderr $ "- no "
                                            <> showt lambdaRuntimeTraceId
                                            <> ": "
                                            <> showt hmap
                                            <> " | " <> showt err
                     logStderr $ showt err
                   Right (evt, hmap) -> do
                     let traceId = lookup lambdaRuntimeTraceId hmap
                     logStderr $ "- s3Event=" <> showt evt
                     logStderr $ "- hmap=" <> showt hmap
                     logStderr $ "- traceId=" <> showt traceId
                     case traceId of
                       Just tid -> set "_X_AMZN_TRACE_ID" (show tid)
                       Nothing -> logStderr $ "- no "
                                            <> showt lambdaRuntimeTraceId
                                            <> ": "
                                            <> showt hmap
                                            <> " | "
                                            <> showt evt
                     case lookup lambdaRuntimeAwsRequestId hmap of
                       Just reqId -> ackEvent (EventId reqId) >>= \r -> logStderr $ "ackEvent response=" <> showt r
                       Nothing -> logStderr $ "- no "
                                            <> showt lambdaRuntimeAwsRequestId
                                            <> ": "
                                            <> showt hmap
                                            <> " | "
                                            <> showt evt
                     return ()
              \err -> logStderr $ "- caught err: " <> showt err

lambdaRuntimeAwsRequestId :: CI Text
lambdaRuntimeAwsRequestId = mk "Lambda-Runtime-Aws-Request-Id"

lambdaRuntimeTraceId :: CI Text
lambdaRuntimeTraceId = mk "Lambda-Runtime-Trace-Id"
