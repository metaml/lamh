module Lambda where

import Control.Lens
import Data.Aeson
import Data.Aeson.Text
import Data.String (IsString)
import Data.Text hiding (drop)
import Data.Text.Conversions
import Event.Event
import GHC.Generics
import Polysemy

data Lambda m a where
  GetEvent :: Lambda m Text
  AckEvent :: EventId -> Lambda m ()
  AckError :: EventId -> Error -> Lambda m ()
  InitError :: Error -> Lambda m ()

makeSem ''Lambda

runLambdaIO :: Member (Lift IO) r => Sem (Lambda ': r) a -> Sem r a
runLambdaIO = interpret $ \case
  GetEvent -> undefined
  AckEvent eid -> undefined
  AckError eid err -> undefined
  InitError err -> undefined
