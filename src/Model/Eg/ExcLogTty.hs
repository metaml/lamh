module Model.Eg.ExcLogTty where

import Prelude hiding (log)
import Control.Exception hiding (catch, throw)
import Polysemy
import Polysemy.Error
import Model.Eg.Tty
import Model.Log
import Util

data Exception' = FooException | BarException | BazException
  deriving (Eq, Exception, Show)

run :: IO ()
run = do
  r <- runM . (runError @Exception') . logToIO . ttyToIO $ program
  print r

program :: Members '[Error Exception', Tty, Log] r => Sem r ()
program = catch @Exception' prg err
  where
    prg = do
      logStdout "- running prg"
      _ <- throw @Exception' FooException
      put' "Hello, World!"
    err = \e -> logStderr $ "- caught error: " <> showt e
