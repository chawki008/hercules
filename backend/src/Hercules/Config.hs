{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE StrictData      #-}
{-# LANGUAGE TemplateHaskell #-}

module Hercules.Config
  ( Config(..)
  , ConnectInfo(..)
  , AuthClientInfo(..)
  , HostName
  , AccessLogLevel(..)
  , BasicAuthInfo (..)
  ) where

import Data.Aeson
import Data.Aeson.TH
import Data.Char                  (toLower)
import Data.Text                  (Text)
import Database.PostgreSQL.Simple (ConnectInfo (..))
import GHC.Generics
import Network.Wai.Handler.Warp   (Port)

import Hercules.OAuth.Types

type HostName = Text

-- | Access logging level
data AccessLogLevel = Disabled | Enabled | Development
  deriving(Read, Show, Generic)

instance FromJSON AccessLogLevel

data Config = Config { configPort                     :: Port
                     , configHostname                 :: HostName
                     , configAccessLogLevel           :: AccessLogLevel
                     , configSecretKeyFile            :: FilePath
                     , configHerculesConnectionString :: Text
                     , configHydraConnectionString    :: Text
                     , configGoogleAuthInfo           :: Maybe AuthClientInfo
                     , configGitHubAuthInfo           :: Maybe AuthClientInfo
                     , configBitbucketBasicAuthInfo   :: Maybe BasicAuthInfo
                     }
  deriving(Read, Show)

-- Derive JSON dropping 'config' and making the first character lowercase.
deriveFromJSON defaultOptions
  { fieldLabelModifier = \s ->
      case drop (length "config") s of
        []   -> []
        x:xs -> toLower x : xs
  }
  ''Config
