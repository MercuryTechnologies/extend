-- | Processor operations for the Extend API
module Extend.V1.Processors
    ( -- * Types
      Processor(..)
    , ProcessorConfig(..)
    , ProcessorRun(..)
    , ProcessorRunStatus(..)
    , GetProcessorResponse(..)
    , ListProcessorsResponse(..)
    , RunProcessorRequest(..)
    , RunProcessorResponse(..)
    , GetProcessorRunResponse(..)
    , ListProcessorRunsResponse(..)
      -- * API
    , ProcessorsAPI
    , getProcessor
    , listProcessors
    , runProcessor
    , getProcessorRun
    , listProcessorRuns
    ) where

import Extend.Prelude
import Extend.V1.Common
import Extend.V1.Files (File)

-- | Processor configuration
newtype ProcessorConfig = ProcessorConfig
    { value :: Value
    -- ^ The raw configuration value
    } deriving stock (Show, Eq, Generic)

instance FromJSON ProcessorConfig where
    parseJSON v = pure $ ProcessorConfig v

instance ToJSON ProcessorConfig where
    toJSON (ProcessorConfig value) = value

-- | A processor in the Extend API
data Processor = Processor
    { object :: ObjectType
    -- ^ Type of the object
    , id :: Text
    -- ^ ID of the processor
    , name :: Text
    -- ^ Name of the processor
    , description :: Maybe Text
    -- ^ Description of the processor
    , version :: Text
    -- ^ Version of the processor
    , config :: ProcessorConfig
    -- ^ Configuration for the processor
    , createdAt :: UTCTime
    -- ^ When the processor was created
    , updatedAt :: UTCTime
    -- ^ When the processor was last updated
    } deriving stock (Show, Eq, Generic)

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
        pure Processor
            { object = objectType
            , id = id
            , name = name
            , description = description
            , version = version
            , config = config
            , createdAt = createdAt
            , updatedAt = updatedAt
            }

instance ToJSON Processor where
    toJSON Processor{..} = object $ catMaybes
        [ Just ("object" .= object)
        , Just ("id" .= id)
        , Just ("name" .= name)
        , ("description" .=) <$> description
        , Just ("version" .= version)
        , Just ("config" .= config)
        , Just ("createdAt" .= createdAt)
        , Just ("updatedAt" .= updatedAt)
        ]

-- | Status of a processor run
data ProcessorRunStatus
    = Pending
    | Processing
    | Processed
    | Failed
    deriving stock (Show, Eq, Generic)

instance FromJSON ProcessorRunStatus where
    parseJSON = withText "ProcessorRunStatus" $ \case
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
    { object :: ObjectType
    -- ^ Type of the object
    , id :: Text
    -- ^ ID of the processor run
    , processorId :: Text
    -- ^ ID of the processor
    , processorVersionId :: Text
    -- ^ ID of the processor version
    , processorName :: Text
    -- ^ Name of the processor
    , status :: ProcessorRunStatus
    -- ^ Status of the processor run
    , config :: ProcessorConfig
    -- ^ Configuration for the processor
    , output :: Value
    -- ^ Output from the processor
    , files :: [File]
    -- ^ Files processed
    , createdAt :: UTCTime
    -- ^ When the processor run was created
    , updatedAt :: UTCTime
    -- ^ When the processor run was last updated
    , failureReason :: Maybe Text
    -- ^ Reason for failure if failed
    , failureMessage :: Maybe Text
    -- ^ Detailed failure message if failed
    } deriving stock (Show, Eq, Generic)

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
        pure ProcessorRun
            { object = objectType
            , id = id
            , processorId = processorId
            , processorVersionId = processorVersionId
            , processorName = processorName
            , status = status
            , config = config
            , output = output
            , files = files
            , createdAt = createdAt
            , updatedAt = updatedAt
            , failureReason = failureReason
            , failureMessage = failureMessage
            }

instance ToJSON ProcessorRun where
    toJSON ProcessorRun{..} = object $ catMaybes
        [ Just ("object" .= object)
        , Just ("id" .= id)
        , Just ("processorId" .= processorId)
        , Just ("processorVersionId" .= processorVersionId)
        , Just ("processorName" .= processorName)
        , Just ("status" .= status)
        , Just ("config" .= config)
        , Just ("output" .= output)
        , Just ("files" .= files)
        , Just ("createdAt" .= createdAt)
        , Just ("updatedAt" .= updatedAt)
        , ("failureReason" .=) <$> failureReason
        , ("failureMessage" .=) <$> failureMessage
        ]

-- | Response from getting a processor
newtype GetProcessorResponse = GetProcessorResponse
    { processor :: Processor
    -- ^ The requested processor
    } deriving stock (Show, Eq, Generic)

instance FromJSON GetProcessorResponse where
    parseJSON = withObject "GetProcessorResponse" $ \v -> do
        processor <- v .: "processor"
        pure GetProcessorResponse{..}

-- | Response from listing processors
data ListProcessorsResponse = ListProcessorsResponse
    { processors :: [Processor]
    -- ^ The processors
    , pagination :: Maybe Pagination
    -- ^ Pagination information
    } deriving stock (Show, Eq, Generic)

instance FromJSON ListProcessorsResponse where
    parseJSON = withObject "ListProcessorsResponse" $ \v -> do
        processors <- v .: "processors"
        pagination <- v .:? "pagination"
        pure ListProcessorsResponse{..}

-- | Request to run a processor
data RunProcessorRequest = RunProcessorRequest
    { fileIds :: [Text]
    -- ^ IDs of the files to process
    , config :: Maybe ProcessorConfig
    -- ^ Optional configuration overrides
    } deriving stock (Show, Eq, Generic)

instance ToJSON RunProcessorRequest where
    toJSON RunProcessorRequest{..} = object $ catMaybes
        [ Just ("fileIds" .= fileIds)
        , ("config" .=) <$> config
        ]

-- | Response from running a processor
newtype RunProcessorResponse = RunProcessorResponse
    { processorRun :: ProcessorRun
    -- ^ The created processor run
    } deriving stock (Show, Eq, Generic)

instance FromJSON RunProcessorResponse where
    parseJSON = withObject "RunProcessorResponse" $ \v -> do
        processorRun <- v .: "processorRun"
        pure RunProcessorResponse{..}

-- | Response from getting a processor run
newtype GetProcessorRunResponse = GetProcessorRunResponse
    { processorRun :: ProcessorRun
    -- ^ The requested processor run
    } deriving stock (Show, Eq, Generic)

instance FromJSON GetProcessorRunResponse where
    parseJSON = withObject "GetProcessorRunResponse" $ \v -> do
        processorRun <- v .: "processorRun"
        pure GetProcessorRunResponse{..}

-- | Response from listing processor runs
data ListProcessorRunsResponse = ListProcessorRunsResponse
    { processorRuns :: [ProcessorRun]
    -- ^ The processor runs
    , pagination :: Maybe Pagination
    -- ^ Pagination information
    } deriving stock (Show, Eq, Generic)

instance FromJSON ListProcessorRunsResponse where
    parseJSON = withObject "ListProcessorRunsResponse" $ \v -> do
        processorRuns <- v .: "processorRuns"
        pagination <- v .:? "pagination"
        pure ListProcessorRunsResponse{..}

-- | Processors API endpoints
type ProcessorsAPI =
    "processors" :> Capture "processorId" Text
        :> Header' '[Required, Strict] "Authorization" Text
        :> Header' '[Required, Strict] "x-extend-api-version" Text
        :> Get '[JSON] (SuccessResponse GetProcessorResponse)
    :<|>
    "processors"
        :> Header' '[Required, Strict] "Authorization" Text
        :> Header' '[Required, Strict] "x-extend-api-version" Text
        :> QueryParam "limit" Int
        :> QueryParam "page" Int
        :> Get '[JSON] (SuccessResponse ListProcessorsResponse)
    :<|>
    "processors" :> Capture "processorId" Text :> "runs"
        :> Header' '[Required, Strict] "Authorization" Text
        :> Header' '[Required, Strict] "x-extend-api-version" Text
        :> ReqBody '[JSON] RunProcessorRequest
        :> Post '[JSON] (SuccessResponse RunProcessorResponse)
    :<|>
    "processors" :> "runs" :> Capture "runId" Text
        :> Header' '[Required, Strict] "Authorization" Text
        :> Header' '[Required, Strict] "x-extend-api-version" Text
        :> Get '[JSON] (SuccessResponse GetProcessorRunResponse)
    :<|>
    "processors" :> Capture "processorId" Text :> "runs"
        :> Header' '[Required, Strict] "Authorization" Text
        :> Header' '[Required, Strict] "x-extend-api-version" Text
        :> QueryParam "limit" Int
        :> QueryParam "page" Int
        :> Get '[JSON] (SuccessResponse ListProcessorRunsResponse)

-- | Get a processor
getProcessor
    :: ApiToken
    -> ApiVersion
    -> Text -- ^ Processor ID
    -> ClientM (SuccessResponse GetProcessorResponse)
getProcessor (ApiToken token) (ApiVersion version) processorId =
    client (Proxy @ProcessorsAPI) processorId ("Bearer " <> token) version

-- | List processors
listProcessors
    :: ApiToken
    -> ApiVersion
    -> Maybe Int -- ^ Limit
    -> Maybe Int -- ^ Page
    -> ClientM (SuccessResponse ListProcessorsResponse)
listProcessors (ApiToken token) (ApiVersion version) =
    client (Proxy @ProcessorsAPI) ("Bearer " <> token) version

-- | Run a processor
runProcessor
    :: ApiToken
    -> ApiVersion
    -> Text -- ^ Processor ID
    -> RunProcessorRequest
    -> ClientM (SuccessResponse RunProcessorResponse)
runProcessor (ApiToken token) (ApiVersion version) processorId =
    client (Proxy @ProcessorsAPI) processorId ("Bearer " <> token) version

-- | Get a processor run
getProcessorRun
    :: ApiToken
    -> ApiVersion
    -> Text -- ^ Run ID
    -> ClientM (SuccessResponse GetProcessorRunResponse)
getProcessorRun (ApiToken token) (ApiVersion version) runId =
    client (Proxy @ProcessorsAPI) runId ("Bearer " <> token) version

-- | List processor runs
listProcessorRuns
    :: ApiToken
    -> ApiVersion
    -> Text -- ^ Processor ID
    -> Maybe Int -- ^ Limit
    -> Maybe Int -- ^ Page
    -> ClientM (SuccessResponse ListProcessorRunsResponse)
listProcessorRuns (ApiToken token) (ApiVersion version) processorId =
    client (Proxy @ProcessorsAPI) processorId ("Bearer " <> token) version 