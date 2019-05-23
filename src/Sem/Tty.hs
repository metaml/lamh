module Sem.Tty where

import Prelude as P
import Polysemy

data Tty m a where
  Get :: Tty m String
  Put :: String -> Tty m ()
  Print :: Show a => a -> Tty m ()
makeSem ''Tty

runTtyIO :: Member (Lift IO) r => Sem (Tty ': r) a -> Sem r a
runTtyIO = interpret $ \case
  Get -> sendM getLine
  Put msg -> sendM $ putStrLn msg
  Print a -> sendM $ P.print a
