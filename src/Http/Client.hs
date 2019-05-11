module Http.Client where

import Data.Proxy
import Data.Text
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API
import Servant.Client
