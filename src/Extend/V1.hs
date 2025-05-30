-- | Extend AI API V1
module Extend.V1
  ( -- * API Client
    getClientEnv,
    baseUrlFromString,

    -- * API Token
    ApiToken (..),

    -- * API Version
    ApiVersion (..),
    defaultApiVersion,

    -- * API Components

    -- Workflows
    Workflow (..),
    WorkflowStep (..),
    WorkflowStepRun (..),
    ValidationRule (..),
    StepRunOutput (..),
    WorkflowRun (..),
    WorkflowRunStatus (..),
    ExtendFile (..),
    PredeterminedOutput (..),
    GetWorkflowResponse (..),
    RunWorkflowRequest (..),
    RunWorkflowResponse (..),
    GetWorkflowRunResponse (..),
    ListWorkflowRunsResponse (..),
    getWorkflow,
    runWorkflow,
    getWorkflowRun,
    listWorkflowRuns,
    -- Files
    module Extend.V1.Files,
    -- Processors
    Processor (..),
    ProcessorConfig (..),
    ProcessorType (..),
    ProcessorRun (..),
    ProcessorRunStatus,
    ProcessorVersion (..),
    ProcessorRunFileInput (..),
    Extend.V1.Processors.MergedProcessor (..),
    RunProcessorRequest (..),
    RunProcessorResponse (..),
    GetProcessorRunResponse (..),
    CreateProcessorRequest (..),
    CreateProcessorResponse (..),
    UpdateProcessorRequest (..),
    UpdateProcessorResponse (..),
    GetProcessorVersionResponse (..),
    ListProcessorVersionsResponse (..),
    PublishProcessorVersionRequest (..),
    PublishProcessorVersionResponse (..),
    BatchProcessorRun (..),
    GetBatchProcessorRunResponse (..),
    runProcessor,
    getProcessorRun,
    createProcessor,
    updateProcessor,
    getProcessorVersion,
    listProcessorVersions,
    publishProcessorVersion,
    getBatchProcessorRun,
    -- Error and Common
    module Extend.V1.Error,
    module Extend.V1.Common,

    -- * API
    ExtendAPI,
  )
where

import Extend.Prelude
import Extend.V1.Common
import Extend.V1.Error
import Extend.V1.Files
import Extend.V1.Processors
import Extend.V1.Workflows
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as TLS
import Servant.Client (BaseUrl (..), Scheme (..))
import qualified Servant.Client as Client

-- | Parse a BaseUrl from a string
baseUrlFromString :: String -> IO BaseUrl
baseUrlFromString baseUrlStr = do
  baseUrl <- Client.parseBaseUrl baseUrlStr
  -- Use HTTPS by default
  let newBaseUrl =
        baseUrl
          { baseUrlScheme = Https,
            baseUrlPort = 443
          }
  pure newBaseUrl

-- | Get a ClientEnv for the Extend API
getClientEnv :: String -> IO ClientEnv
getClientEnv baseUrlStr = do
  baseUrl <- baseUrlFromString baseUrlStr
  let managerSettings =
        TLS.tlsManagerSettings
          { HTTP.managerResponseTimeout = HTTP.responseTimeoutNone
          }
  manager <- TLS.newTlsManagerWith managerSettings
  pure (Client.mkClientEnv manager baseUrl)

-- | API definition
type ExtendAPI =
  FilesAPI :<|> ProcessorsAPI :<|> WorkflowsAPI