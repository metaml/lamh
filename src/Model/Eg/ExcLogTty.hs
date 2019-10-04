module Model.Eg.ExcLogTty where

import Prelude hiding (log)
import Colog.Core
import Colog.Polysemy
import Control.Exception hiding (catch, throw)
import Polysemy
import Polysemy.Error
import Sem.Tty

data MyException = FooException | BarException | BazException
  deriving (Eq, Show)

instance Exception MyException

run' :: IO ()
run' = do
  r <- runM $ (runError @MyException) . (runLogAction @IO logStringStderr) . runTtyIO $ program
  print r
  return ()

program :: Members '[Error MyException, Tty, Log String] r => Sem r ()
program = catch @MyException prg err where
            prg = do
              log "- prg"
              _ <- throw @MyException FooException
              put' "Hello, World!"
            err = \e -> log @String ("- log | error: " <> show e)
