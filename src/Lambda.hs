module Lambda where

import Prelude hiding (log)
import Data.List.Split
import Colog.Core.IO (logStringStderr)
import Colog.Polysemy (runLogAction)
import Colog.Polysemy.Effect
import Control.Exception hiding (catch, throw)
import Network.HTTP.Client
import Polysemy
import Polysemy.Error
import Polysemy.Reader
import Sem.Lambda
import Servant.Client (BaseUrl(..), Scheme(..))
import System.Environment

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
  r <- (runError @SomeException) . runLambdaIO $ echoEvent
  case r of
    Left x -> log $ "failure: " <> show x
    Right _ -> log @String "success"
  return ()

echoEvent :: Members '[Error SomeException, Log String, Lambda] r => Sem r ()
echoEvent = catch @SomeException
              do getS3EventPair >>= \case
                   Left e -> log $ show e
                   Right (evt, evtId) -> do
                     log $ "evtId=" <> show evtId
                     log $ "s3Event=" <> show evt
                     r <- ackEvent evtId
                     log $ "ackEvent response=" <> show r
                     return ()
              \err -> log $ show err
