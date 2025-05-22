{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- | Processor operations for the Extend API
module Extend.V1.Processors
  ( -- * Types
    Processor (..),
    ProcessorConfig (..),
    ProcessorType (..),
    ProcessorRun (..),
    ProcessorRunStatus (..),
    ProcessorVersion (..),
    ProcessorRunFileInput (..),
    MergedProcessor (..),
    GetProcessorResponse (..),
    RunProcessorRequest (..),
    RunProcessorResponse (..),
    GetProcessorRunResponse (..),
    BatchProcessorRun (..),
    GetBatchProcessorRunResponse (..),
    CreateProcessorRequest (..),
    CreateProcessorResponse (..),
    UpdateProcessorRequest (..),
    UpdateProcessorResponse (..),
    GetProcessorVersionResponse (..),
    ListProcessorVersionsResponse (..),
    PublishProcessorVersionRequest (..),
    PublishProcessorVersionResponse (..),

    -- * API
    ProcessorsAPI,
    runProcessor,
    getProcessorRun,
    createProcessor,
    updateProcessor,
    getProcessorVersion,
    listProcessorVersions,
    publishProcessorVersion,
    getBatchProcessorRun,
  )
where

import Data.Aeson ((.!=))
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

-- | Type of processor
data ProcessorType
  = Extract
  | Classify
  | Splitter
  deriving stock (Show, Eq, Generic)

instance FromJSON ProcessorType where
  parseJSON = Aeson.withText "ProcessorType" $ \case
    "EXTRACT" -> pure Extract
    "CLASSIFY" -> pure Classify
    "SPLITTER" -> pure Splitter
    _ -> fail "Unknown processor type"

instance ToJSON ProcessorType where
  toJSON = \case
    Extract -> String "EXTRACT"
    Classify -> String "CLASSIFY"
    Splitter -> String "SPLITTER"

-- | A processor in the Extend API
data Processor = Processor
  { -- | Type of the object
    processorObject :: ObjectType,
    -- | ID of the processor
    processorId :: Text,
    -- | Name of the processor
    processorName :: Text,
    -- | Type of the processor
    processorType :: ProcessorType,
    -- | When the processor was created
    processorCreatedAt :: UTCTime,
    -- | When the processor was last updated
    processorUpdatedAt :: UTCTime,
    -- | Draft version of the processor
    processorDraftVersion :: Maybe ProcessorVersion
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON Processor where
  parseJSON = withObject "Processor" $ \v -> do
    objectType <- v .: "object"
    id <- v .: "id"
    name <- v .: "name"
    processorType <- v .: "type"
    createdAt <- v .: "createdAt"
    updatedAt <- v .: "updatedAt"
    draftVersion <- v .:? "draftVersion"
    pure
      Processor
        { processorObject = objectType,
          processorId = id,
          processorName = name,
          processorType = processorType,
          processorCreatedAt = createdAt,
          processorUpdatedAt = updatedAt,
          processorDraftVersion = draftVersion
        }

instance ToJSON Processor where
  toJSON Processor {..} =
    Aeson.object $
      catMaybes
        [ Just ("object" .= processorObject),
          Just ("id" .= processorId),
          Just ("name" .= processorName),
          Just ("type" .= processorType),
          Just ("createdAt" .= processorCreatedAt),
          Just ("updatedAt" .= processorUpdatedAt),
          ("draftVersion" .=) <$> processorDraftVersion
        ]

-- | A processor version
data ProcessorVersion = ProcessorVersion
  { -- | Type of the object
    processorVersionObject :: ObjectType,
    -- | ID of the processor version
    processorVersionId :: Text,
    -- | ID of the processor
    processorVersionProcessorId :: Text,
    -- | Name of the processor
    processorVersionProcessorName :: Maybe Text,
    -- | Type of the processor
    processorVersionProcessorType :: ProcessorType,
    -- | Description of the processor version
    processorVersionDescription :: Maybe Text,
    -- | Version of the processor
    processorVersionVersion :: Text,
    -- | Configuration for the processor
    processorVersionConfig :: ProcessorConfig,
    -- | When the processor version was created
    processorVersionCreatedAt :: UTCTime,
    -- | When the processor version was last updated
    processorVersionUpdatedAt :: UTCTime
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ProcessorVersion where
  parseJSON = withObject "ProcessorVersion" $ \v -> do
    objectType <- v .: "object"
    id <- v .: "id"
    processorId <- v .: "processorId"
    processorName <- v .:? "processorName"
    processorType <- v .: "processorType"
    description <- v .:? "description"
    version <- v .: "version"
    config <- v .: "config"
    createdAt <- v .: "createdAt"
    updatedAt <- v .: "updatedAt"
    pure
      ProcessorVersion
        { processorVersionObject = objectType,
          processorVersionId = id,
          processorVersionProcessorId = processorId,
          processorVersionProcessorName = processorName,
          processorVersionProcessorType = processorType,
          processorVersionDescription = description,
          processorVersionVersion = version,
          processorVersionConfig = config,
          processorVersionCreatedAt = createdAt,
          processorVersionUpdatedAt = updatedAt
        }

instance ToJSON ProcessorVersion where
  toJSON ProcessorVersion {..} =
    Aeson.object $
      catMaybes
        [ Just ("object" .= processorVersionObject),
          Just ("id" .= processorVersionId),
          Just ("processorId" .= processorVersionProcessorId),
          ("processorName" .=) <$> processorVersionProcessorName,
          Just ("processorType" .= processorVersionProcessorType),
          ("description" .=) <$> processorVersionDescription,
          Just ("version" .= processorVersionVersion),
          Just ("config" .= processorVersionConfig),
          Just ("createdAt" .= processorVersionCreatedAt),
          Just ("updatedAt" .= processorVersionUpdatedAt)
        ]

-- | Status of a processor run
data ProcessorRunStatus
  = Processing
  | Processed
  | Failed
  deriving stock (Show, Eq, Generic)

instance FromJSON ProcessorRunStatus where
  parseJSON = Aeson.withText "ProcessorRunStatus" $ \case
    "PROCESSING" -> pure Processing
    "PROCESSED" -> pure Processed
    "FAILED" -> pure Failed
    _ -> fail "Unknown processor run status"

instance ToJSON ProcessorRunStatus where
  toJSON = \case
    Processing -> String "PROCESSING"
    Processed -> String "PROCESSED"
    Failed -> String "FAILED"

-- | A merged processor reference in processor run outputs
data MergedProcessor = MergedProcessor
  { -- | ID of the processor
    mergedProcessorProcessorId :: Text,
    -- | ID of the processor version
    mergedProcessorProcessorVersionId :: Text,
    -- | Name of the processor
    mergedProcessorProcessorName :: Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON MergedProcessor where
  parseJSON = Aeson.withObject "MergedProcessor" $ \v -> do
    processorId <- v Aeson..: "processorId"
    processorVersionId <- v Aeson..: "processorVersionId"
    processorName <- v Aeson..: "processorName"
    pure
      MergedProcessor
        { mergedProcessorProcessorId = processorId,
          mergedProcessorProcessorVersionId = processorVersionId,
          mergedProcessorProcessorName = processorName
        }

instance ToJSON MergedProcessor where
  toJSON MergedProcessor {..} =
    Aeson.object
      [ "processorId" .= mergedProcessorProcessorId,
        "processorVersionId" .= mergedProcessorProcessorVersionId,
        "processorName" .= mergedProcessorProcessorName
      ]

-- | A processor run in the Extend API
data ProcessorRun = ProcessorRun
  { -- | Type of the object
    processorRunObject :: ObjectType,
    -- | ID of the processor run
    processorRunId :: Text,
    -- | ID of the processor
    processorRunProcessorId :: Text,
    -- | ID of the processor version
    processorRunProcessorVersionId :: Text,
    -- | Name of the processor
    processorRunProcessorName :: Text,
    -- | Status of the processor run
    processorRunStatus :: ProcessorRunStatus,
    -- | Output from the processor
    processorRunOutput :: Value,
    -- | Reason for failure if failed
    processorRunFailureReason :: Maybe Text,
    -- | Detailed failure message if failed
    processorRunFailureMessage :: Maybe Text,
    -- | Metadata for the processor run
    processorRunMetadata :: Maybe Value,
    -- | Whether the run has been reviewed
    processorRunReviewed :: Bool,
    -- | Whether the run has been edited
    processorRunEdited :: Bool,
    -- | Edits made to the output
    processorRunEdits :: Value,
    -- | Type of processor
    processorRunType :: Maybe Text,
    -- | Configuration for the processor
    processorRunConfig :: ProcessorConfig,
    -- | Initial output from the processor
    processorRunInitialOutput :: Maybe Value,
    -- | Reviewed output from the processor
    processorRunReviewedOutput :: Maybe Value,
    -- | Files processed
    processorRunFiles :: [File],
    -- | Merged processors
    processorRunMergedProcessors :: [MergedProcessor],
    -- | Dashboard URL for the processor run
    processorRunUrl :: Maybe Text
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
    output <- v .: "output"
    failureReason <- v .:? "failureReason"
    failureMessage <- v .:? "failureMessage"
    metadata <- v .:? "metadata"
    reviewed <- v .: "reviewed"
    edited <- v .: "edited"
    edits <- v .: "edits"
    type_ <- v .:? "type"
    config <- v .: "config"
    initialOutput <- v .:? "initialOutput"
    reviewedOutput <- v .:? "reviewedOutput"
    files <- v .: "files"
    mergedProcessors <- v .:? "mergedProcessors" .!= []
    url <- v .:? "url"
    pure
      ProcessorRun
        { processorRunObject = objectType,
          processorRunId = id,
          processorRunProcessorId = processorId,
          processorRunProcessorVersionId = processorVersionId,
          processorRunProcessorName = processorName,
          processorRunStatus = status,
          processorRunOutput = output,
          processorRunFailureReason = failureReason,
          processorRunFailureMessage = failureMessage,
          processorRunMetadata = metadata,
          processorRunReviewed = reviewed,
          processorRunEdited = edited,
          processorRunEdits = edits,
          processorRunType = type_,
          processorRunConfig = config,
          processorRunInitialOutput = initialOutput,
          processorRunReviewedOutput = reviewedOutput,
          processorRunFiles = files,
          processorRunMergedProcessors = mergedProcessors,
          processorRunUrl = url
        }

instance ToJSON ProcessorRun where
  toJSON ProcessorRun {..} =
    Aeson.object $
      catMaybes
        [ Just ("object" .= processorRunObject),
          Just ("id" .= processorRunId),
          Just ("processorId" .= processorRunProcessorId),
          Just ("processorVersionId" .= processorRunProcessorVersionId),
          Just ("processorName" .= processorRunProcessorName),
          Just ("status" .= processorRunStatus),
          Just ("output" .= processorRunOutput),
          ("failureReason" .=) <$> processorRunFailureReason,
          ("failureMessage" .=) <$> processorRunFailureMessage,
          ("metadata" .=) <$> processorRunMetadata,
          Just ("reviewed" .= processorRunReviewed),
          Just ("edited" .= processorRunEdited),
          Just ("edits" .= processorRunEdits),
          ("type" .=) <$> processorRunType,
          Just ("config" .= processorRunConfig),
          ("initialOutput" .=) <$> processorRunInitialOutput,
          ("reviewedOutput" .=) <$> processorRunReviewedOutput,
          Just ("files" .= processorRunFiles),
          Just ("mergedProcessors" .= processorRunMergedProcessors),
          ("url" .=) <$> processorRunUrl
        ]

-- | File to process through a processor
data ProcessorRunFileInput = ProcessorRunFileInput
  { -- | The name of the file to be processed
    processorRunFileInputFileName :: Maybe Text,
    -- | A URL where the file can be downloaded from
    processorRunFileInputFileUrl :: Maybe Text,
    -- | Extend's internal ID for the file
    processorRunFileInputFileId :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ProcessorRunFileInput where
  parseJSON = Aeson.withObject "ProcessorRunFileInput" $ \v -> do
    fileName <- v Aeson..:? "fileName"
    fileUrl <- v Aeson..:? "fileUrl"
    fileId <- v Aeson..:? "fileId"
    pure
      ProcessorRunFileInput
        { processorRunFileInputFileName = fileName,
          processorRunFileInputFileUrl = fileUrl,
          processorRunFileInputFileId = fileId
        }

instance ToJSON ProcessorRunFileInput where
  toJSON ProcessorRunFileInput {..} =
    Aeson.object $
      catMaybes
        [ ("fileName" .=) <$> processorRunFileInputFileName,
          ("fileUrl" .=) <$> processorRunFileInputFileUrl,
          ("fileId" .=) <$> processorRunFileInputFileId
        ]

-- | Response from getting a processor
data GetProcessorResponse = GetProcessorResponse
  { -- | Whether the request was successful
    getProcessorResponseSuccess :: Bool,
    -- | The requested processor
    getProcessorResponseProcessor :: Processor
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON GetProcessorResponse where
  parseJSON = withObject "GetProcessorResponse" $ \v -> do
    success <- v .: "success"
    processor <- v .: "processor"
    pure
      GetProcessorResponse
        { getProcessorResponseSuccess = success,
          getProcessorResponseProcessor = processor
        }

-- | Request to run a processor
data RunProcessorRequest = RunProcessorRequest
  { -- | The ID of the processor to run
    runProcessorRequestProcessorId :: Text,
    -- | Optional version of the processor to use
    runProcessorRequestVersion :: Maybe Text,
    -- | Optional file to process
    runProcessorRequestFile :: Maybe ProcessorRunFileInput,
    -- | Optional raw text to process
    runProcessorRequestRawText :: Maybe Text,
    -- | Optional priority
    runProcessorRequestPriority :: Maybe Int,
    -- | Optional metadata
    runProcessorRequestMetadata :: Maybe Value,
    -- | Optional configuration overrides
    runProcessorRequestConfig :: Maybe ProcessorConfig
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON RunProcessorRequest where
  toJSON RunProcessorRequest {..} =
    Aeson.object $
      catMaybes
        [ Just ("processorId" .= runProcessorRequestProcessorId),
          ("version" .=) <$> runProcessorRequestVersion,
          ("file" .=) <$> runProcessorRequestFile,
          ("rawText" .=) <$> runProcessorRequestRawText,
          ("priority" .=) <$> runProcessorRequestPriority,
          ("metadata" .=) <$> runProcessorRequestMetadata,
          ("config" .=) <$> runProcessorRequestConfig
        ]

instance FromJSON RunProcessorRequest where
  parseJSON = Aeson.withObject "RunProcessorRequest" $ \v -> do
    processorId <- v Aeson..: "processorId"
    version <- v Aeson..:? "version"
    file <- v Aeson..:? "file"
    rawText <- v Aeson..:? "rawText"
    priority <- v Aeson..:? "priority"
    metadata <- v Aeson..:? "metadata"
    config <- v Aeson..:? "config"
    pure
      RunProcessorRequest
        { runProcessorRequestProcessorId = processorId,
          runProcessorRequestVersion = version,
          runProcessorRequestFile = file,
          runProcessorRequestRawText = rawText,
          runProcessorRequestPriority = priority,
          runProcessorRequestMetadata = metadata,
          runProcessorRequestConfig = config
        }

-- | Response from running a processor
data RunProcessorResponse = RunProcessorResponse
  { -- | Whether the request was successful
    runProcessorResponseSuccess :: Bool,
    -- | The created processor run
    runProcessorResponseProcessorRun :: ProcessorRun
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON RunProcessorResponse where
  parseJSON = withObject "RunProcessorResponse" $ \v -> do
    success <- v .: "success"
    processorRun <- v .: "processorRun"
    pure
      RunProcessorResponse
        { runProcessorResponseSuccess = success,
          runProcessorResponseProcessorRun = processorRun
        }

instance ToJSON RunProcessorResponse where
  toJSON RunProcessorResponse {..} =
    Aeson.object
      [ "success" .= runProcessorResponseSuccess,
        "processorRun" .= runProcessorResponseProcessorRun
      ]

-- | Response from getting a processor run
data GetProcessorRunResponse = GetProcessorRunResponse
  { -- | Whether the request was successful
    getProcessorRunResponseSuccess :: Bool,
    -- | The requested processor run
    getProcessorRunResponseProcessorRun :: ProcessorRun
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON GetProcessorRunResponse where
  parseJSON = withObject "GetProcessorRunResponse" $ \v -> do
    success <- v .: "success"
    processorRun <- v .: "processorRun"
    pure
      GetProcessorRunResponse
        { getProcessorRunResponseSuccess = success,
          getProcessorRunResponseProcessorRun = processorRun
        }

instance ToJSON GetProcessorRunResponse where
  toJSON GetProcessorRunResponse {..} =
    Aeson.object
      [ "success" .= getProcessorRunResponseSuccess,
        "processorRun" .= getProcessorRunResponseProcessorRun
      ]

-- | A batch processor run
data BatchProcessorRun = BatchProcessorRun
  { -- | Type of the object
    batchProcessorRunObject :: ObjectType,
    -- | ID of the batch processor run
    batchProcessorRunId :: Text,
    -- | ID of the processor
    batchProcessorRunProcessorId :: Text,
    -- | ID of the processor version
    batchProcessorRunProcessorVersionId :: Text,
    -- | Name of the processor
    batchProcessorRunProcessorName :: Text,
    -- | Metrics for the batch run
    batchProcessorRunMetrics :: Value,
    -- | Status of the batch processor run
    batchProcessorRunStatus :: Text,
    -- | Source of the batch processor run
    batchProcessorRunSource :: Text,
    -- | ID of the source
    batchProcessorRunSourceId :: Maybe Text,
    -- | Number of runs
    batchProcessorRunRunCount :: Int,
    -- | Options for the batch processor run
    batchProcessorRunOptions :: Value,
    -- | When the batch processor run was created
    batchProcessorRunCreatedAt :: UTCTime,
    -- | When the batch processor run was last updated
    batchProcessorRunUpdatedAt :: UTCTime
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON BatchProcessorRun where
  parseJSON = withObject "BatchProcessorRun" $ \v -> do
    objectType <- v .: "object"
    id <- v .: "id"
    processorId <- v .: "processorId"
    processorVersionId <- v .: "processorVersionId"
    processorName <- v .: "processorName"
    metrics <- v .: "metrics"
    status <- v .: "status"
    source <- v .: "source"
    sourceId <- v .:? "sourceId"
    runCount <- v .: "runCount"
    options <- v .: "options"
    createdAt <- v .: "createdAt"
    updatedAt <- v .: "updatedAt"
    pure
      BatchProcessorRun
        { batchProcessorRunObject = objectType,
          batchProcessorRunId = id,
          batchProcessorRunProcessorId = processorId,
          batchProcessorRunProcessorVersionId = processorVersionId,
          batchProcessorRunProcessorName = processorName,
          batchProcessorRunMetrics = metrics,
          batchProcessorRunStatus = status,
          batchProcessorRunSource = source,
          batchProcessorRunSourceId = sourceId,
          batchProcessorRunRunCount = runCount,
          batchProcessorRunOptions = options,
          batchProcessorRunCreatedAt = createdAt,
          batchProcessorRunUpdatedAt = updatedAt
        }

instance ToJSON BatchProcessorRun where
  toJSON BatchProcessorRun {..} =
    Aeson.object $
      catMaybes
        [ Just ("object" .= batchProcessorRunObject),
          Just ("id" .= batchProcessorRunId),
          Just ("processorId" .= batchProcessorRunProcessorId),
          Just ("processorVersionId" .= batchProcessorRunProcessorVersionId),
          Just ("processorName" .= batchProcessorRunProcessorName),
          Just ("metrics" .= batchProcessorRunMetrics),
          Just ("status" .= batchProcessorRunStatus),
          Just ("source" .= batchProcessorRunSource),
          ("sourceId" .=) <$> batchProcessorRunSourceId,
          Just ("runCount" .= batchProcessorRunRunCount),
          Just ("options" .= batchProcessorRunOptions),
          Just ("createdAt" .= batchProcessorRunCreatedAt),
          Just ("updatedAt" .= batchProcessorRunUpdatedAt)
        ]

-- | Response from getting a batch processor run
data GetBatchProcessorRunResponse = GetBatchProcessorRunResponse
  { -- | Whether the request was successful
    getBatchProcessorRunResponseSuccess :: Bool,
    -- | The requested batch processor run
    getBatchProcessorRunResponseBatchProcessorRun :: BatchProcessorRun
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON GetBatchProcessorRunResponse where
  parseJSON = withObject "GetBatchProcessorRunResponse" $ \v -> do
    success <- v .: "success"
    batchProcessorRun <- v .: "batchProcessorRun"
    pure
      GetBatchProcessorRunResponse
        { getBatchProcessorRunResponseSuccess = success,
          getBatchProcessorRunResponseBatchProcessorRun = batchProcessorRun
        }

instance ToJSON GetBatchProcessorRunResponse where
  toJSON GetBatchProcessorRunResponse {..} =
    Aeson.object
      [ "success" .= getBatchProcessorRunResponseSuccess,
        "batchProcessorRun" .= getBatchProcessorRunResponseBatchProcessorRun
      ]

-- | Request to create a processor
data CreateProcessorRequest = CreateProcessorRequest
  { -- | The name of the processor
    createProcessorRequestName :: Text,
    -- | The type of the processor
    createProcessorRequestType :: ProcessorType,
    -- | The ID of an existing processor to clone
    createProcessorRequestCloneProcessorId :: Maybe Text,
    -- | Configuration for the processor
    createProcessorRequestConfig :: Maybe ProcessorConfig
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON CreateProcessorRequest where
  toJSON CreateProcessorRequest {..} =
    Aeson.object $
      catMaybes
        [ Just ("name" .= createProcessorRequestName),
          Just ("type" .= createProcessorRequestType),
          ("cloneProcessorId" .=) <$> createProcessorRequestCloneProcessorId,
          ("config" .=) <$> createProcessorRequestConfig
        ]

instance FromJSON CreateProcessorRequest where
  parseJSON = Aeson.withObject "CreateProcessorRequest" $ \v -> do
    name <- v Aeson..: "name"
    type_ <- v Aeson..: "type"
    cloneProcessorId <- v Aeson..:? "cloneProcessorId"
    config <- v Aeson..:? "config"
    pure
      CreateProcessorRequest
        { createProcessorRequestName = name,
          createProcessorRequestType = type_,
          createProcessorRequestCloneProcessorId = cloneProcessorId,
          createProcessorRequestConfig = config
        }

-- | Response from creating a processor
data CreateProcessorResponse = CreateProcessorResponse
  { -- | Whether the request was successful
    createProcessorResponseSuccess :: Bool,
    -- | The created processor
    createProcessorResponseProcessor :: Processor
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON CreateProcessorResponse where
  parseJSON = withObject "CreateProcessorResponse" $ \v -> do
    success <- v .: "success"
    processor <- v .: "processor"
    pure
      CreateProcessorResponse
        { createProcessorResponseSuccess = success,
          createProcessorResponseProcessor = processor
        }

instance ToJSON CreateProcessorResponse where
  toJSON CreateProcessorResponse {..} =
    Aeson.object
      [ "success" .= createProcessorResponseSuccess,
        "processor" .= createProcessorResponseProcessor
      ]

-- | Request to update a processor
data UpdateProcessorRequest = UpdateProcessorRequest
  { -- | The name of the processor
    updateProcessorRequestName :: Maybe Text,
    -- | Configuration for the processor
    updateProcessorRequestConfig :: Maybe ProcessorConfig
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON UpdateProcessorRequest where
  toJSON UpdateProcessorRequest {..} =
    Aeson.object $
      catMaybes
        [ ("name" .=) <$> updateProcessorRequestName,
          ("config" .=) <$> updateProcessorRequestConfig
        ]

instance FromJSON UpdateProcessorRequest where
  parseJSON = Aeson.withObject "UpdateProcessorRequest" $ \v -> do
    name <- v Aeson..:? "name"
    config <- v Aeson..:? "config"
    pure
      UpdateProcessorRequest
        { updateProcessorRequestName = name,
          updateProcessorRequestConfig = config
        }

-- | Response from updating a processor
data UpdateProcessorResponse = UpdateProcessorResponse
  { -- | Whether the request was successful
    updateProcessorResponseSuccess :: Bool,
    -- | The updated processor
    updateProcessorResponseProcessor :: Processor
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON UpdateProcessorResponse where
  parseJSON = withObject "UpdateProcessorResponse" $ \v -> do
    success <- v .: "success"
    processor <- v .: "processor"
    pure
      UpdateProcessorResponse
        { updateProcessorResponseSuccess = success,
          updateProcessorResponseProcessor = processor
        }

instance ToJSON UpdateProcessorResponse where
  toJSON UpdateProcessorResponse {..} =
    Aeson.object
      [ "success" .= updateProcessorResponseSuccess,
        "processor" .= updateProcessorResponseProcessor
      ]

-- | Response from getting a processor version
data GetProcessorVersionResponse = GetProcessorVersionResponse
  { -- | Whether the request was successful
    getProcessorVersionResponseSuccess :: Bool,
    -- | The requested processor version
    getProcessorVersionResponseVersion :: ProcessorVersion
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON GetProcessorVersionResponse where
  parseJSON = withObject "GetProcessorVersionResponse" $ \v -> do
    success <- v .: "success"
    version <- v .: "version"
    pure
      GetProcessorVersionResponse
        { getProcessorVersionResponseSuccess = success,
          getProcessorVersionResponseVersion = version
        }

instance ToJSON GetProcessorVersionResponse where
  toJSON GetProcessorVersionResponse {..} =
    Aeson.object
      [ "success" .= getProcessorVersionResponseSuccess,
        "version" .= getProcessorVersionResponseVersion
      ]

-- | Response from listing processor versions
data ListProcessorVersionsResponse = ListProcessorVersionsResponse
  { -- | Whether the request was successful
    listProcessorVersionsResponseSuccess :: Bool,
    -- | The processor versions
    listProcessorVersionsResponseVersions :: [ProcessorVersion]
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ListProcessorVersionsResponse where
  parseJSON = withObject "ListProcessorVersionsResponse" $ \v -> do
    success <- v .: "success"
    versions <- v .: "versions"
    pure
      ListProcessorVersionsResponse
        { listProcessorVersionsResponseSuccess = success,
          listProcessorVersionsResponseVersions = versions
        }

instance ToJSON ListProcessorVersionsResponse where
  toJSON ListProcessorVersionsResponse {..} =
    Aeson.object
      [ "success" .= listProcessorVersionsResponseSuccess,
        "versions" .= listProcessorVersionsResponseVersions
      ]

-- | Request to publish a processor version
data PublishProcessorVersionRequest = PublishProcessorVersionRequest
  { -- | The type of release for this version
    publishProcessorVersionRequestReleaseType :: Text,
    -- | Description of the changes in this version
    publishProcessorVersionRequestDescription :: Maybe Text,
    -- | Configuration for this version of the processor
    publishProcessorVersionRequestConfig :: Maybe ProcessorConfig
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON PublishProcessorVersionRequest where
  toJSON PublishProcessorVersionRequest {..} =
    Aeson.object $
      catMaybes
        [ Just ("releaseType" .= publishProcessorVersionRequestReleaseType),
          ("description" .=) <$> publishProcessorVersionRequestDescription,
          ("config" .=) <$> publishProcessorVersionRequestConfig
        ]

instance FromJSON PublishProcessorVersionRequest where
  parseJSON = Aeson.withObject "PublishProcessorVersionRequest" $ \v -> do
    releaseType <- v Aeson..: "releaseType"
    description <- v Aeson..:? "description"
    config <- v Aeson..:? "config"
    pure
      PublishProcessorVersionRequest
        { publishProcessorVersionRequestReleaseType = releaseType,
          publishProcessorVersionRequestDescription = description,
          publishProcessorVersionRequestConfig = config
        }

-- | Response from publishing a processor version
data PublishProcessorVersionResponse = PublishProcessorVersionResponse
  { -- | Whether the request was successful
    publishProcessorVersionResponseSuccess :: Bool,
    -- | The published processor version
    publishProcessorVersionResponseProcessorVersion :: ProcessorVersion
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON PublishProcessorVersionResponse where
  parseJSON = withObject "PublishProcessorVersionResponse" $ \v -> do
    success <- v .: "success"
    processorVersion <- v .: "processorVersion"
    pure
      PublishProcessorVersionResponse
        { publishProcessorVersionResponseSuccess = success,
          publishProcessorVersionResponseProcessorVersion = processorVersion
        }

instance ToJSON PublishProcessorVersionResponse where
  toJSON PublishProcessorVersionResponse {..} =
    Aeson.object
      [ "success" .= publishProcessorVersionResponseSuccess,
        "processorVersion" .= publishProcessorVersionResponseProcessorVersion
      ]

-- | Processors API endpoints
type ProcessorsAPI =
  "processor_runs"
    :> Header' '[Required, Strict] "Authorization" Text
    :> Header' '[Required, Strict] "x-extend-api-version" Text
    :> ReqBody '[JSON] RunProcessorRequest
    :> Post '[JSON] RunProcessorResponse
    :<|> "processor_runs"
    :> Capture "id" Text
    :> Header' '[Required, Strict] "Authorization" Text
    :> Header' '[Required, Strict] "x-extend-api-version" Text
    :> Get '[JSON] GetProcessorRunResponse
    :<|> "processors"
    :> Header' '[Required, Strict] "Authorization" Text
    :> Header' '[Required, Strict] "x-extend-api-version" Text
    :> ReqBody '[JSON] CreateProcessorRequest
    :> Post '[JSON] CreateProcessorResponse
    :<|> "processors"
    :> Capture "id" Text
    :> Header' '[Required, Strict] "Authorization" Text
    :> Header' '[Required, Strict] "x-extend-api-version" Text
    :> ReqBody '[JSON] UpdateProcessorRequest
    :> Post '[JSON] UpdateProcessorResponse
    :<|> "processors"
    :> Capture "id" Text
    :> "versions"
    :> Capture "processorVersionId" Text
    :> Header' '[Required, Strict] "Authorization" Text
    :> Header' '[Required, Strict] "x-extend-api-version" Text
    :> Get '[JSON] GetProcessorVersionResponse
    :<|> "processors"
    :> Capture "id" Text
    :> "versions"
    :> Header' '[Required, Strict] "Authorization" Text
    :> Header' '[Required, Strict] "x-extend-api-version" Text
    :> Get '[JSON] ListProcessorVersionsResponse
    :<|> "processors"
    :> Capture "id" Text
    :> "publish"
    :> Header' '[Required, Strict] "Authorization" Text
    :> Header' '[Required, Strict] "x-extend-api-version" Text
    :> ReqBody '[JSON] PublishProcessorVersionRequest
    :> Post '[JSON] PublishProcessorVersionResponse
    :<|> "batch_processor_runs"
    :> Capture "id" Text
    :> Header' '[Required, Strict] "Authorization" Text
    :> Header' '[Required, Strict] "x-extend-api-version" Text
    :> Get '[JSON] GetBatchProcessorRunResponse

-- | Split the client functions for easier access
processorsAPI :: Proxy ProcessorsAPI
processorsAPI = Proxy

runProcessorClient :: Text -> Text -> RunProcessorRequest -> ClientM RunProcessorResponse
getProcessorRunClient :: Text -> Text -> Text -> ClientM GetProcessorRunResponse
createProcessorClient :: Text -> Text -> CreateProcessorRequest -> ClientM CreateProcessorResponse
updateProcessorClient :: Text -> Text -> Text -> UpdateProcessorRequest -> ClientM UpdateProcessorResponse
getProcessorVersionClient :: Text -> Text -> Text -> Text -> ClientM GetProcessorVersionResponse
listProcessorVersionsClient :: Text -> Text -> Text -> ClientM ListProcessorVersionsResponse
publishProcessorVersionClient :: Text -> Text -> Text -> PublishProcessorVersionRequest -> ClientM PublishProcessorVersionResponse
getBatchProcessorRunClient :: Text -> Text -> Text -> ClientM GetBatchProcessorRunResponse
runProcessorClient :<|> getProcessorRunClient :<|> createProcessorClient :<|> updateProcessorClient :<|> getProcessorVersionClient :<|> listProcessorVersionsClient :<|> publishProcessorVersionClient :<|> getBatchProcessorRunClient = client processorsAPI

-- | Run a processor
runProcessor ::
  ApiToken ->
  ApiVersion ->
  RunProcessorRequest ->
  ClientM RunProcessorResponse
runProcessor (ApiToken token) (ApiVersion version) = runProcessorClient ("Bearer " <> token) version

-- | Get a processor run
getProcessorRun ::
  ApiToken ->
  ApiVersion ->
  -- | Run ID
  Text ->
  ClientM GetProcessorRunResponse
getProcessorRun (ApiToken token) (ApiVersion version) runId =
  getProcessorRunClient runId ("Bearer " <> token) version

-- | Create a processor
createProcessor ::
  ApiToken ->
  ApiVersion ->
  CreateProcessorRequest ->
  ClientM CreateProcessorResponse
createProcessor (ApiToken token) (ApiVersion version) = createProcessorClient ("Bearer " <> token) version

-- | Update a processor
updateProcessor ::
  ApiToken ->
  ApiVersion ->
  -- | Processor ID
  Text ->
  UpdateProcessorRequest ->
  ClientM UpdateProcessorResponse
updateProcessor (ApiToken token) (ApiVersion version) processorId = updateProcessorClient processorId ("Bearer " <> token) version

-- | Get a processor version
getProcessorVersion ::
  ApiToken ->
  ApiVersion ->
  -- | Processor ID
  Text ->
  -- | Processor version ID
  Text ->
  ClientM GetProcessorVersionResponse
getProcessorVersion (ApiToken token) (ApiVersion version) processorId processorVersionId =
  getProcessorVersionClient processorId processorVersionId ("Bearer " <> token) version

-- | List processor versions
listProcessorVersions ::
  ApiToken ->
  ApiVersion ->
  -- | Processor ID
  Text ->
  ClientM ListProcessorVersionsResponse
listProcessorVersions (ApiToken token) (ApiVersion version) processorId =
  listProcessorVersionsClient processorId ("Bearer " <> token) version

-- | Publish a processor version
publishProcessorVersion ::
  ApiToken ->
  ApiVersion ->
  -- | Processor ID
  Text ->
  PublishProcessorVersionRequest ->
  ClientM PublishProcessorVersionResponse
publishProcessorVersion (ApiToken token) (ApiVersion version) processorId = publishProcessorVersionClient processorId ("Bearer " <> token) version

-- | Get a batch processor run
getBatchProcessorRun ::
  ApiToken ->
  ApiVersion ->
  -- | Batch processor run ID
  Text ->
  ClientM GetBatchProcessorRunResponse
getBatchProcessorRun (ApiToken token) (ApiVersion version) batchProcessorRunId =
  getBatchProcessorRunClient
    batchProcessorRunId
    ("Bearer " <> token)
    version