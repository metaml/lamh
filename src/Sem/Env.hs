module Sem.Env where

import Polysemy
import System.Environment

type Key = String
type Val = String

data Env m a where
  Get :: Key -> Env m (Maybe Val)
  Set :: Key -> Val -> Env m ()

makeSem ''Env

runEnvIO :: Member (Lift IO) r => Sem (Env ': r) a -> Sem r a
runEnvIO = interpret $ \case
  Get k -> sendM $ lookupEnv k
  Set k v -> sendM $ setEnv k v
