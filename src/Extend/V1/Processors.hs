-- | Processor operations for the Extend API
module Extend.V1.Processors
  ( -- * Types
    Processor (..),
    ProcessorConfig (..),
    ProcessorRun (..),
    ProcessorRunStatus (..),
    GetProcessorResponse (..),
    ListProcessorsResponse (..),
    RunProcessorRequest (..),
    RunProcessorResponse (..),
    GetProcessorRunResponse (..),
    ListProcessorRunsResponse (..),

    -- * API
    ProcessorsAPI,
    getProcessor,
    listProcessors,
    runProcessor,
    getProcessorRun,
    listProcessorRuns,
  )
where

import qualified Data.Aeson as Aeson
import Data.Proxy (Proxy (..))
import Extend.Prelude
import Extend.V1.Common
import Extend.V1.Files (File)

-- | Processor configuration
newtype ProcessorConfig = ProcessorConfig
  { -- | The raw configuration value
    value :: Value
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ProcessorConfig where
  parseJSON v = pure $ ProcessorConfig v

instance ToJSON ProcessorConfig where
  toJSON (ProcessorConfig value) = value

-- | A processor in the Extend API
data Processor = Processor
  { -- | Type of the object
    object :: ObjectType,
    -- | ID of the processor
    id :: Text,
    -- | Name of the processor
    name :: Text,
    -- | Description of the processor
    description :: Maybe Text,
    -- | Version of the processor
    version :: Text,
    -- | Configuration for the processor
    config :: ProcessorConfig,
    -- | When the processor was created
    createdAt :: UTCTime,
    -- | When the processor was last updated
    updatedAt :: UTCTime
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON Processor where
  parseJSON = withObject "Processor" $ \v -> do
    objectType <- v .: "object"
    id <- v .: "id"
    name <- v .: "name"
    description <- v .:? "description"
    version <- v .: "version"
    config <- v .: "config"
    createdAt <- v .: "createdAt"
    updatedAt <- v .: "updatedAt"
    pure
      Processor
        { object = objectType,
          id = id,
          name = name,
          description = description,
          version = version,
          config = config,
          createdAt = createdAt,
          updatedAt = updatedAt
        }

instance ToJSON Processor where
  toJSON Processor {..} =
    Aeson.object $
      catMaybes
        [ Just ("object" .= object),
          Just ("id" .= id),
          Just ("name" .= name),
          ("description" .=) <$> description,
          Just ("version" .= version),
          Just ("config" .= config),
          Just ("createdAt" .= createdAt),
          Just ("updatedAt" .= updatedAt)
        ]

-- | Status of a processor run
data ProcessorRunStatus
  = Pending
  | Processing
  | Processed
  | Failed
  deriving stock (Show, Eq, Generic)

instance FromJSON ProcessorRunStatus where
  parseJSON = Aeson.withText "ProcessorRunStatus" $ \case
    "PENDING" -> pure Pending
    "PROCESSING" -> pure Processing
    "PROCESSED" -> pure Processed
    "FAILED" -> pure Failed
    _ -> fail "Unknown processor run status"

instance ToJSON ProcessorRunStatus where
  toJSON = \case
    Pending -> String "PENDING"
    Processing -> String "PROCESSING"
    Processed -> String "PROCESSED"
    Failed -> String "FAILED"

-- | A processor run in the Extend API
data ProcessorRun = ProcessorRun
  { -- | Type of the object
    object :: ObjectType,
    -- | ID of the processor run
    id :: Text,
    -- | ID of the processor
    processorId :: Text,
    -- | ID of the processor version
    processorVersionId :: Text,
    -- | Name of the processor
    processorName :: Text,
    -- | Status of the processor run
    status :: ProcessorRunStatus,
    -- | Configuration for the processor
    config :: ProcessorConfig,
    -- | Output from the processor
    output :: Value,
    -- | Files processed
    files :: [File],
    -- | When the processor run was created
    createdAt :: UTCTime,
    -- | When the processor run was last updated
    updatedAt :: UTCTime,
    -- | Reason for failure if failed
    failureReason :: Maybe Text,
    -- | Detailed failure message if failed
    failureMessage :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ProcessorRun where
  parseJSON = withObject "ProcessorRun" $ \v -> do
    objectType <- v .: "object"
    id <- v .: "id"
    processorId <- v .: "processorId"
    processorVersionId <- v .: "processorVersionId"
    processorName <- v .: "processorName"
    status <- v .: "status"
    config <- v .: "config"
    output <- v .: "output"
    files <- v .: "files"
    createdAt <- v .: "createdAt"
    updatedAt <- v .: "updatedAt"
    failureReason <- v .:? "failureReason"
    failureMessage <- v .:? "failureMessage"
    pure
      ProcessorRun
        { object = objectType,
          id = id,
          processorId = processorId,
          processorVersionId = processorVersionId,
          processorName = processorName,
          status = status,
          config = config,
          output = output,
          files = files,
          createdAt = createdAt,
          updatedAt = updatedAt,
          failureReason = failureReason,
          failureMessage = failureMessage
        }

instance ToJSON ProcessorRun where
  toJSON ProcessorRun {..} =
    Aeson.object $
      catMaybes
        [ Just ("object" .= object),
          Just ("id" .= id),
          Just ("processorId" .= processorId),
          Just ("processorVersionId" .= processorVersionId),
          Just ("processorName" .= processorName),
          Just ("status" .= status),
          Just ("config" .= config),
          Just ("output" .= output),
          Just ("files" .= files),
          Just ("createdAt" .= createdAt),
          Just ("updatedAt" .= updatedAt),
          ("failureReason" .=) <$> failureReason,
          ("failureMessage" .=) <$> failureMessage
        ]

-- | Response from getting a processor
newtype GetProcessorResponse = GetProcessorResponse
  { -- | The requested processor
    processor :: Processor
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON GetProcessorResponse where
  parseJSON = withObject "GetProcessorResponse" $ \v -> do
    processor <- v .: "processor"
    pure GetProcessorResponse {..}

-- | Response from listing processors
data ListProcessorsResponse = ListProcessorsResponse
  { -- | The processors
    processors :: [Processor],
    -- | Pagination information
    pagination :: Maybe Pagination
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ListProcessorsResponse where
  parseJSON = withObject "ListProcessorsResponse" $ \v -> do
    processors <- v .: "processors"
    pagination <- v .:? "pagination"
    pure ListProcessorsResponse {..}

-- | Request to run a processor
data RunProcessorRequest = RunProcessorRequest
  { -- | IDs of the files to process
    fileIds :: [Text],
    -- | Optional configuration overrides
    config :: Maybe ProcessorConfig
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON RunProcessorRequest where
  toJSON RunProcessorRequest {..} =
    Aeson.object $
      catMaybes
        [ Just ("fileIds" .= fileIds),
          ("config" .=) <$> config
        ]

-- | Response from running a processor
newtype RunProcessorResponse = RunProcessorResponse
  { -- | The created processor run
    processorRun :: ProcessorRun
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON RunProcessorResponse where
  parseJSON = withObject "RunProcessorResponse" $ \v -> do
    processorRun <- v .: "processorRun"
    pure RunProcessorResponse {..}

-- | Response from getting a processor run
newtype GetProcessorRunResponse = GetProcessorRunResponse
  { -- | The requested processor run
    processorRun :: ProcessorRun
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON GetProcessorRunResponse where
  parseJSON = withObject "GetProcessorRunResponse" $ \v -> do
    processorRun <- v .: "processorRun"
    pure GetProcessorRunResponse {..}

-- | Response from listing processor runs
data ListProcessorRunsResponse = ListProcessorRunsResponse
  { -- | The processor runs
    processorRuns :: [ProcessorRun],
    -- | Pagination information
    pagination :: Maybe Pagination
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ListProcessorRunsResponse where
  parseJSON = withObject "ListProcessorRunsResponse" $ \v -> do
    processorRuns <- v .: "processorRuns"
    pagination <- v .:? "pagination"
    pure ListProcessorRunsResponse {..}

-- | Processors API endpoints
type ProcessorsAPI =
  "processors"
    :> Capture "processorId" Text
    :> Header' '[Required, Strict] "Authorization" Text
    :> Header' '[Required, Strict] "x-extend-api-version" Text
    :> Get '[JSON] (SuccessResponse GetProcessorResponse)
    :<|> "processors"
    :> Header' '[Required, Strict] "Authorization" Text
    :> Header' '[Required, Strict] "x-extend-api-version" Text
    :> QueryParam "limit" Int
    :> QueryParam "page" Int
    :> Get '[JSON] (SuccessResponse ListProcessorsResponse)
    :<|> "processors"
    :> Capture "processorId" Text
    :> "runs"
    :> Header' '[Required, Strict] "Authorization" Text
    :> Header' '[Required, Strict] "x-extend-api-version" Text
    :> ReqBody '[JSON] RunProcessorRequest
    :> Post '[JSON] (SuccessResponse RunProcessorResponse)
    :<|> "processors"
    :> "runs"
    :> Capture "runId" Text
    :> Header' '[Required, Strict] "Authorization" Text
    :> Header' '[Required, Strict] "x-extend-api-version" Text
    :> Get '[JSON] (SuccessResponse GetProcessorRunResponse)
    :<|> "processors"
    :> Capture "processorId" Text
    :> "runs"
    :> Header' '[Required, Strict] "Authorization" Text
    :> Header' '[Required, Strict] "x-extend-api-version" Text
    :> QueryParam "limit" Int
    :> QueryParam "page" Int
    :> Get '[JSON] (SuccessResponse ListProcessorRunsResponse)

-- | Split the client functions for easier access
processorsAPI :: Proxy ProcessorsAPI
processorsAPI = Proxy

getProcessorClient :: Text -> Text -> Text -> ClientM (SuccessResponse GetProcessorResponse)
listProcessorsClient :: Text -> Text -> Maybe Int -> Maybe Int -> ClientM (SuccessResponse ListProcessorsResponse)
runProcessorClient :: Text -> Text -> Text -> RunProcessorRequest -> ClientM (SuccessResponse RunProcessorResponse)
getProcessorRunClient :: Text -> Text -> Text -> ClientM (SuccessResponse GetProcessorRunResponse)
listProcessorRunsClient :: Text -> Text -> Text -> Maybe Int -> Maybe Int -> ClientM (SuccessResponse ListProcessorRunsResponse)
getProcessorClient :<|> listProcessorsClient :<|> runProcessorClient :<|> getProcessorRunClient :<|> listProcessorRunsClient = client processorsAPI

-- | Get a processor
getProcessor ::
  ApiToken ->
  ApiVersion ->
  -- | Processor ID
  Text ->
  ClientM (SuccessResponse GetProcessorResponse)
getProcessor (ApiToken token) (ApiVersion version) processorId =
  getProcessorClient processorId ("Bearer " <> token) version

-- | List processors
listProcessors ::
  ApiToken ->
  ApiVersion ->
  -- | Limit
  Maybe Int ->
  -- | Page
  Maybe Int ->
  ClientM (SuccessResponse ListProcessorsResponse)
listProcessors (ApiToken token) (ApiVersion version) limit page =
  listProcessorsClient ("Bearer " <> token) version limit page

-- | Run a processor
runProcessor ::
  ApiToken ->
  ApiVersion ->
  -- | Processor ID
  Text ->
  RunProcessorRequest ->
  ClientM (SuccessResponse RunProcessorResponse)
runProcessor (ApiToken token) (ApiVersion version) processorId req =
  runProcessorClient processorId ("Bearer " <> token) version req

-- | Get a processor run
getProcessorRun ::
  ApiToken ->
  ApiVersion ->
  -- | Run ID
  Text ->
  ClientM (SuccessResponse GetProcessorRunResponse)
getProcessorRun (ApiToken token) (ApiVersion version) runId =
  getProcessorRunClient runId ("Bearer " <> token) version

-- | List processor runs
listProcessorRuns ::
  ApiToken ->
  ApiVersion ->
  -- | Processor ID
  Text ->
  -- | Limit
  Maybe Int ->
  -- | Page
  Maybe Int ->
  ClientM (SuccessResponse ListProcessorRunsResponse)
listProcessorRuns (ApiToken token) (ApiVersion version) processorId limit page =
  listProcessorRunsClient processorId ("Bearer " <> token) version limit page