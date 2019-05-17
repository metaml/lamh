module Eg where

import Data.Aeson
import Data.Proxy
import Data.Text
import GHC.Generics
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Polysemy
import Servant.API
import Servant.Client

awsUrl :: BaseUrl
awsUrl = BaseUrl Http "hackage.haskell.org" 80 ""

-- servant

newtype Package = Package { packageName :: Text }
  deriving (Eq, Generic, Show)
  deriving anyclass (FromJSON)

type Api = "packages" :> Get '[JSON] [Package]
api :: Proxy Api
api = Proxy

packages :: ClientM [Package]
packages = client api

get' :: BaseUrl -> IO [Package]
get' url = do
  mgr <- newManager defaultManagerSettings
  r <- runClientM packages (mkClientEnv mgr url)
  case r of
    Left err -> print err >> return []
    Right ps -> return ps

-- polysemy

data Http m a where
  Get :: Http m [Package]

makeSem ''Http

getPackages :: Member Http r => Sem r [Package]
getPackages = do
  ps <- get
  return ps

runHttpIO :: Member (Lift IO) r => Sem (Http ': r) a -> Sem r a
runHttpIO = interpret $ \case
  Get -> sendM $ get' awsUrl

getPackagesIO :: Sem '[Lift IO] [Package]
getPackagesIO = runHttpIO getPackages
