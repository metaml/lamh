module Lambda where

import Prelude hiding (print)
import Polysemy
import Sem.Lambda
import Sem.Tty

echo :: Members '[Tty, Lambda] r => Sem r ()
echo = getEvent >>= print

echoIO :: Sem '[Lift IO] ()
echoIO = undefined
