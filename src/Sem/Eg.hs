{-# language FlexibleInstances, MultiParamTypeClasses #-}
module Sem.Eg where

import Data.Aeson
import Data.Proxy
import Data.Text hiding (take)
import Data.Typeable
import GHC.Generics
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Polysemy
import Servant.API
import Servant.API.ContentTypes (eitherDecodeLenient)
import Servant.Client
import Network.HTTP.Media ((//))

-- hackage data-type models
data User = User { username :: Text
                 , userid :: Int
                 }
  deriving (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

newtype Package = Package { packageName :: Text }
  deriving (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

-- polysemy
hackageUrl :: BaseUrl
hackageUrl = BaseUrl Http "hackage.haskell.org" 80 ""

data Hackage m a where
  GetPackages :: Hackage m [Package]
  GetUsers :: Hackage m [User]

makeSem ''Hackage

runHackageIO :: Member (Lift IO) r => Sem (Hackage ': r) a -> Sem r a
runHackageIO = interpret $ \case
  GetPackages -> sendM getPackages'
  GetUsers -> sendM getUsers'

packagesIO :: Sem '[Lift IO] [Package]
packagesIO = runHackageIO getPackages

usersIO :: Sem '[Lift IO] [User]
usersIO = runHackageIO getUsers

-- servant-client
-- NB bug: hackge.haskell.org doesn't recognize servant's JSON accept header,
--         "Accept: application/json;charset=utf-8,application/json"
-- thus JSON' hack below
data JSON' deriving Typeable

instance Accept JSON' where
  contentType _ = "application" // "json"

instance ToJSON a => MimeRender JSON' a where
  mimeRender _ = encode

instance FromJSON a => MimeUnrender JSON' a where
  mimeUnrender _ = eitherDecodeLenient

type HackageApi = "packages" :> Get '[JSON'] [Package]
                  :<|> "users" :> Get '[JSON'] [User]
hackageApi :: Proxy HackageApi
hackageApi = Proxy

packages :: ClientM [Package]
users :: ClientM [User]
packages :<|> users = client hackageApi

getPackages' :: IO [Package]
getPackages' = do
  mgr <- newManager defaultManagerSettings
  let env = mkClientEnv mgr hackageUrl
  runClientM packages env >>= \case
    Left err -> print (take 256 $ show err) >> return []
    Right ps -> return ps

getUsers' :: IO [User]
getUsers' = do
  mgr <- newManager defaultManagerSettings
  let env = mkClientEnv mgr hackageUrl
  runClientM users env >>= \case
    Left err -> print (take 256 $ show err) >> return []
    Right us -> return us
