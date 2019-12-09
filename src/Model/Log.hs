module Model.Log where

import Colog
import Data.Text as T
import Polysemy

data Log m a where
  LogStdout :: Text -> Log m ()
  LogStderr :: Text -> Log m ()
  LogStdout' :: [Text] -> Log m ()
  LogStderr' :: [Text] -> Log m ()

makeSem ''Log

logToIO :: Member (Embed IO) r => Sem (Log ': r) a -> Sem r a
logToIO = interpret $ \case
  LogStdout x -> embed $ log' logTextStdout x
  LogStderr x -> embed $ log' logTextStderr x
  LogStdout' xs -> embed $ log' logTextStdout $ T.unwords xs
  LogStderr' xs -> embed $ log' logTextStderr $ T.unwords xs
  where
    log' :: LogAction IO Text -> Text -> IO ()
    log' logger msg = logger <& msg
