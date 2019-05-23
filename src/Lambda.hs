module Lambda where

import Prelude hiding (print)
import Network.HTTP.Client
import Polysemy
import Polysemy.Reader
import Sem.Lambda
import Sem.Tty
import Servant.Client (BaseUrl(..), Scheme(..))
import System.Environment

echoEvent :: Members '[Tty, Lambda] r => Sem r ()
echoEvent = getEvent >>= print

runEchoEventIO :: Sem '[Reader BaseUrl, Reader Manager, Lift IO] ()
runEchoEventIO = runLambdaIO . runTtyIO $ echoEvent

echoEventIO :: IO ()
echoEventIO = do
  hostname <- getEnv "AWS_LAMBDA_RUNTIME_API"
  manager <- newManager defaultManagerSettings
  let baseUrl = BaseUrl Http hostname 80 "2018-06-01/runtime"
  runM $ runReader manager . runReader baseUrl $ runEchoEventIO
  return ()
