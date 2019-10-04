module Model.Log where

import Colog
import Data.Text
import Polysemy

data Log m a where
  LogStdout :: Text -> Log m ()
  LogStderr :: Text -> Log m ()

makeSem ''Log

runLogIO :: Member (Embed IO) r => Sem (Log ': r) a -> Sem r a
runLogIO = interpret $ \case
  LogStdout x -> embed $ log' logTextStdout x
  LogStderr x -> embed $ log' logTextStderr x
  where
    log' :: LogAction IO Text -> Text -> IO ()
    log' logger msg = logger <& msg
