-- | Extend AI API V1
module Extend.V1
    ( -- * API Client
      getClientEnv
    , baseUrlFromString
      -- * API Token
    , ApiToken(..)
      -- * API Version
    , ApiVersion(..)
    , defaultApiVersion
      -- * API Components
    , module Extend.V1.Workflows
    , module Extend.V1.Files
    , module Extend.V1.Processors
    , module Extend.V1.Error
    , module Extend.V1.Common
      -- * API
    , ExtendAPI
    ) where

import Control.Exception qualified as Exception
import Data.Proxy (Proxy(..))
import Extend.Prelude
import Extend.V1.Common
import Extend.V1.Error
import Extend.V1.Files (FilesAPI)
import Extend.V1.Files
import Extend.V1.Processors (ProcessorsAPI)
import Extend.V1.Processors
import Extend.V1.Workflows (WorkflowsAPI)
import Extend.V1.Workflows
import Network.HTTP.Client.TLS qualified as TLS
import Servant.Client (BaseUrl(..), ClientEnv, Scheme(..))
import Servant.Client qualified as Client

-- | Parse a BaseUrl from a string
baseUrlFromString :: String -> IO BaseUrl
baseUrlFromString baseUrlStr = do
    baseUrl <- Client.parseBaseUrl baseUrlStr
    -- Use HTTPS by default
    let newBaseUrl = baseUrl
          { baseUrlScheme = Https
          , baseUrlPort = 443
          }
    pure newBaseUrl

-- | Get a ClientEnv for the Extend API
getClientEnv :: String -> IO ClientEnv
getClientEnv baseUrlStr = do
    baseUrl <- baseUrlFromString baseUrlStr
    let managerSettings = TLS.tlsManagerSettings
            { Client.managerResponseTimeout =
                Client.responseTimeoutNone
            }
    manager <- TLS.newTlsManagerWith managerSettings
    pure (Client.mkClientEnv manager baseUrl)

-- | API definition
type ExtendAPI = 
    FilesAPI :<|> ProcessorsAPI :<|> WorkflowsAPI 