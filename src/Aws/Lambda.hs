module Aws.Lambda where

import Control.Lens
import Data.Aeson
import Data.Aeson.Text
import Data.String (IsString)
import Data.Text hiding (drop)
import Data.Text.Conversions
import GHC.Generics
import Polysemy

newtype EventId = EventId Text
  deriving (Eq, Show)
  deriving newtype (IsString, ToText)

data Error = Error { _errorType :: Text
                   , _errorMessage :: Text
                   } deriving (Eq, Generic, Show)

instance ToJSON Error where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON Error where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

makeLenses ''Error

data Lambda m a where
  GetEvent :: Lambda m Text
  AckEvent :: EventId -> Lambda m ()
  ErrEvent :: EventId -> Error -> Lambda m ()
  InitError :: Error -> Lambda m ()

makeSem ''Lambda

runLambdaIO :: Member (Lift IO) r => Sem (Lambda ': r) a -> Sem r a
runLambdaIO = interpret $ \case
  GetEvent -> undefined
  AckEvent eid -> undefined
  ErrEvent eid err -> undefined
  InitError err -> undefined
