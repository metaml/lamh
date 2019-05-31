module Sem.Log where

import Prelude hiding (log)
import Colog.Polysemy.Effect
import Polysemy

runLogIO :: Members '[Log String, Lift IO] => Sem (Log ': r a) -> Sem r ()
runLogIO = runLogAction @IO logStringStdErr

log' msg = log @String msg
