module Lambda where

import Prelude hiding (log, lookup)
import Data.CaseInsensitive (CI, mk)
import Event.Event hiding (Error)
import Data.HashMap.Strict (lookup)
import Data.List.Split
import Data.Text hiding (splitOn)
import Colog.Core.IO (logStringStderr)
import Colog.Polysemy (runLogAction)
import Colog.Polysemy.Effect
import Control.Exception hiding (catch, throw)
import Network.HTTP.Client
import Polysemy
import Polysemy.Error
import Polysemy.Reader
import Sem.Env
import Sem.Lambda
import Servant.Client (BaseUrl(..), Scheme(..))
import System.Environment

lambdaRuntimeAwsRequestId :: CI Text
lambdaRuntimeAwsRequestId = mk "Lambda-Runtime-Aws-Request-Id"

lambdaRuntimeTraceId :: CI Text
lambdaRuntimeTraceId = mk "Lambda-Runtime-Trace-Id"

echoEventIO :: IO ()
echoEventIO = do
  hostport <- getEnv "AWS_LAMBDA_RUNTIME_API"
  manager <- newManager defaultManagerSettings
  let ps = splitOn ":" hostport
      (hostname, port') = (ps !! 0, read (ps !! 1) :: Int)
      baseUrl = BaseUrl Http hostname port' ""
  runM $ (runLogAction @IO logStringStderr) . runReader manager . runReader baseUrl $ runEchoEventIO

runEchoEventIO :: Sem '[Reader BaseUrl, Reader Manager, Log String, Lift IO] ()
runEchoEventIO = do
  r <- (runError @SomeException) . runEnvIO . runLambdaIO $ echoEvent
  case r of
    Left x -> log $ "failure: " <> show x
    Right _ -> log @String "success"
  return ()

echoEvent :: Members '[Error SomeException, Log String, Env, Lambda] r => Sem r ()
echoEvent = catch @SomeException
              do getS3EventPair >>= \case
                   Left (err, hmap) -> do
                     let traceId = lookup lambdaRuntimeTraceId hmap
                     case traceId of
                       Just tid -> set "_X_AMZN_TRACE_ID" (show tid)
                       Nothing -> log $ "- no " <> show lambdaRuntimeTraceId <> ": " <> show hmap <> " | " <> show err
                     log $ show err
                   Right (evt, hmap) -> do
                     log $ "- s3Event=" <> show evt
                     log $ "- hmap=" <> show hmap
                     let traceId = lookup lambdaRuntimeTraceId hmap
                     log $ "- traceId=" <> show traceId
                     case traceId of
                       Just tid -> set "_X_AMZN_TRACE_ID" (show tid)
                       Nothing -> log $ "- no " <> show lambdaRuntimeTraceId <> ": " <> show hmap <> " | " <> show evt
                     let evtId = lookup lambdaRuntimeAwsRequestId hmap
                     case evtId of
                       Just eid -> ackEvent (EventId eid) >>= \r -> log $ "ackEvent response=" <> show r
                       Nothing -> log $ "- no " <> show lambdaRuntimeAwsRequestId <> ": " <> show hmap <> " | " <> show evt
                     return ()
              \err -> log $ "caught err: " <> show err
