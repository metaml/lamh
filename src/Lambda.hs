module Lambda where

import Prelude hiding (log)
import Data.List.Split
import Colog.Core.IO (logStringStderr)
import Colog.Polysemy (runLogAction)
import Colog.Polysemy.Effect
import Control.Exception hiding (catch)
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
  let (hostname, port') = (ps !! 0, read (ps !! 1) :: Int) where ps = splitOn ":" hostport
      baseUrl = BaseUrl Http hostname port' ""
  runM $ (runLogAction @IO logStringStderr) . runReader manager . runReader baseUrl $ runEchoEventIO

runEchoEventIO :: Sem '[Reader BaseUrl, Reader Manager, Log String, Lift IO] ()
runEchoEventIO = do
  e <- (runError @SomeException) . runLambdaIO $ echoEvent
  case e of
    Left exc -> log @String $ "failure: " <> show exc
    Right () -> log @String "success"
  return ()

echoEvent :: Members '[Error SomeException, Log String, Lambda] r => Sem r ()
echoEvent = catch @SomeException
              (getS3Event >>= \evt -> log @String $ show evt)
              (\err -> log @String $ show err)
