{-# language FlexibleInstances, MultiParamTypeClasses #-}
module Model.Eg.Hackage where

import Prelude as P
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
data Hackage m a where
  GetPkgs :: Hackage m [Package]
  GetUsers :: Hackage m [User]

data Tty m a where
  Put :: Show a => a -> Tty m ()

makeSem ''Hackage
makeSem ''Tty

putPkgs :: Members '[Tty, Hackage] r => Sem r ()
putPkgs = getPkgs >>= put

runTtyIO :: Member (Embed IO) r => Sem (Tty ': r) a -> Sem r a
runTtyIO = interpret $ \case
  Put a -> embed $ P.print a

runHackageIO :: Member (Embed IO) r => Sem (Hackage ': r) a -> Sem r a
runHackageIO = interpret $ \case
  GetPkgs -> embed getPkgs'
  GetUsers -> embed getUsers'

putPkgsIO :: Sem '[Embed IO] ()
putPkgsIO = runHackageIO . runTtyIO $ putPkgs

putPkgsIO' :: Sem '[Embed IO] ()
putPkgsIO' = runTtyIO . runHackageIO $ putPkgs

-- runM <x>IO
pkgsIO :: Sem '[Embed IO] [Package]
pkgsIO = runHackageIO getPkgs

usersIO :: Sem '[Embed IO] [User]
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

pkgs :: ClientM [Package]
users :: ClientM [User]
pkgs :<|> users = client hackageApi

hackageUrl :: BaseUrl
hackageUrl = BaseUrl Http "hackage.haskell.org" 80 ""

getPkgs' :: IO [Package]
getPkgs' = do
  mgr <- newManager defaultManagerSettings
  let env = mkClientEnv mgr hackageUrl
  runClientM pkgs env >>= \case
    Left err -> P.print err >> return []
    Right ps -> return ps

getUsers' :: IO [User]
getUsers' = do
  mgr <- newManager defaultManagerSettings
  let env = mkClientEnv mgr hackageUrl
  runClientM users env >>= \case
    Left err -> P.print err >> return []
    Right us -> return us
