module Model.Env where

import Polysemy
import qualified System.Environment as E

type Key = String
type Val = String

data Env m a where
  GetEnv :: Key -> Env m (Maybe Val)
  SetEnv :: Key -> Val -> Env m ()

makeSem ''Env

envToIO :: Member (Embed IO) r => Sem (Env ': r) a -> Sem r a
envToIO = interpret $ \case
  GetEnv k -> embed $ E.lookupEnv k
  SetEnv k v -> embed $ E.setEnv k v
