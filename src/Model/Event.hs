module Model.Event where

import Data.HashMap.Strict
import Data.Text
import Data.CaseInsensitive as CI
import Event.S3
import GHC.Generics
import Servant.Client.Core.ClientError

type Key = Text
type Val = Text
type HeaderMap = HashMap (CI Key) Val

data ClientError' = ClientError' { error :: ClientError
                                 , header :: HeaderMap
                                 } deriving (Eq, Generic, Show)

data S3Event' = S3Event' { event :: S3Event
                         , header :: HeaderMap
                         } deriving (Eq, Generic, Show)
