{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

-- | Workflow operations for the Extend API
module Extend.V1.Workflows
  ( -- * Types
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
    ListWorkflowsResponse (..),
    RunWorkflowRequest (..),
    RunWorkflowResponse (..),
    GetWorkflowRunResponse (..),
    ListWorkflowRunsResponse (..),

    -- * API
    WorkflowsAPI,
    getWorkflow,
    listWorkflows,
    runWorkflow,
    getWorkflowRun,
    listWorkflowRuns,
  )
where

import qualified Data.Aeson as Aeson
import Data.Proxy (Proxy (..))
import Extend.Prelude
import Extend.V1.Common
import Extend.V1.Files (File)
import Extend.V1.Processors (ProcessorRun)

-- | A workflow step
data WorkflowStep = WorkflowStep
  { -- | Type of the object
    object :: ObjectType,
    -- | ID of the step
    id :: Text,
    -- | Name of the step
    name :: Text,
    -- | Type of the step
    type_ :: Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON WorkflowStep where
  parseJSON = Aeson.withObject "WorkflowStep" $ \v -> do
    objectType <- v Aeson..: "object"
    id <- v Aeson..: "id"
    name <- v Aeson..: "name"
    type_ <- v Aeson..: "type"
    pure
      WorkflowStep
        { object = objectType,
          id = id,
          name = name,
          type_ = type_
        }

instance ToJSON WorkflowStep where
  toJSON WorkflowStep {..} =
    Aeson.object
      [ "object" .= object,
        "id" .= id,
        "name" .= name,
        "type" .= type_
      ]

-- | Validation rule for a workflow step
data ValidationRule = ValidationRule
  { -- | Name of the rule
    name :: Text,
    -- | Whether the rule is valid
    valid :: Bool,
    -- | Array of valid values
    validArray :: Maybe [Text],
    -- | Reason for failure
    failureReason :: Maybe Text,
    -- | Error message
    error :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ValidationRule where
  parseJSON = Aeson.withObject "ValidationRule" $ \v -> do
    name <- v Aeson..: "name"
    valid <- v Aeson..: "valid"
    validArray <- v Aeson..:? "validArray"
    failureReason <- v Aeson..:? "failureReason"
    error <- v Aeson..:? "error"
    pure ValidationRule {..}

instance ToJSON ValidationRule where
  toJSON ValidationRule {..} =
    Aeson.object $
      catMaybes
        [ Just ("name" .= name),
          Just ("valid" .= valid),
          ("validArray" .=) <$> validArray,
          ("failureReason" .=) <$> failureReason,
          ("error" .=) <$> error
        ]

-- | Output of a workflow step run
data StepRunOutput = StepRunOutput
  { -- | Validation rules
    rules :: Maybe [ValidationRule]
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON StepRunOutput where
  parseJSON = Aeson.withObject "StepRunOutput" $ \v -> do
    rules <- v Aeson..:? "rules"
    pure StepRunOutput {..}

instance ToJSON StepRunOutput where
  toJSON StepRunOutput {..} =
    Aeson.object $
      catMaybes
        [ ("rules" .=) <$> rules
        ]

-- | A workflow step run
data WorkflowStepRun = WorkflowStepRun
  { -- | Type of the object
    object :: ObjectType,
    -- | ID of the step run
    id :: Text,
    -- | Status of the step run
    status :: Text,
    -- | The step that was run
    step :: WorkflowStep,
    -- | Output from the step
    output :: Maybe StepRunOutput
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON WorkflowStepRun where
  parseJSON = Aeson.withObject "WorkflowStepRun" $ \v -> do
    objectType <- v Aeson..: "object"
    id <- v Aeson..: "id"
    status <- v Aeson..: "status"
    step <- v Aeson..: "step"
    output <- v Aeson..:? "output"
    pure
      WorkflowStepRun
        { object = objectType,
          id = id,
          status = status,
          step = step,
          output = output
        }

instance ToJSON WorkflowStepRun where
  toJSON WorkflowStepRun {..} =
    Aeson.object $
      catMaybes
        [ Just ("object" .= object),
          Just ("id" .= id),
          Just ("status" .= status),
          Just ("step" .= step),
          ("output" .=) <$> output
        ]

-- | A workflow
data Workflow = Workflow
  { -- | Type of the object
    object :: ObjectType,
    -- | ID of the workflow
    id :: Text,
    -- | Name of the workflow
    name :: Text,
    -- | Description of the workflow
    description :: Maybe Text,
    -- | Version of the workflow
    version :: Text,
    -- | When the workflow was created
    createdAt :: UTCTime,
    -- | When the workflow was last updated
    updatedAt :: UTCTime
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON Workflow where
  parseJSON = Aeson.withObject "Workflow" $ \v -> do
    objectType <- v Aeson..: "object"
    id <- v Aeson..: "id"
    name <- v Aeson..: "name"
    description <- v Aeson..:? "description"
    version <- v Aeson..: "version"
    createdAt <- v Aeson..: "createdAt"
    updatedAt <- v Aeson..: "updatedAt"
    pure
      Workflow
        { object = objectType,
          id = id,
          name = name,
          description = description,
          version = version,
          createdAt = createdAt,
          updatedAt = updatedAt
        }

instance ToJSON Workflow where
  toJSON Workflow {..} =
    Aeson.object $
      catMaybes
        [ Just ("object" .= object),
          Just ("id" .= id),
          Just ("name" .= name),
          ("description" .=) <$> description,
          Just ("version" .= version),
          Just ("createdAt" .= createdAt),
          Just ("updatedAt" .= updatedAt)
        ]

-- | Status of a workflow run
data WorkflowRunStatus
  = Pending
  | Processing
  | NeedsReview
  | Rejected
  | Processed
  | Failed
  deriving stock (Show, Eq, Generic)

instance FromJSON WorkflowRunStatus where
  parseJSON = Aeson.withText "WorkflowRunStatus" $ \case
    "PENDING" -> pure Pending
    "PROCESSING" -> pure Processing
    "NEEDS_REVIEW" -> pure NeedsReview
    "REJECTED" -> pure Rejected
    "PROCESSED" -> pure Processed
    "FAILED" -> pure Failed
    _ -> fail "Unknown workflow run status"

instance ToJSON WorkflowRunStatus where
  toJSON = \case
    Pending -> String "PENDING"
    Processing -> String "PROCESSING"
    NeedsReview -> String "NEEDS_REVIEW"
    Rejected -> String "REJECTED"
    Processed -> String "PROCESSED"
    Failed -> String "FAILED"

-- | A workflow run
data WorkflowRun = WorkflowRun
  { -- | Type of the object
    object :: ObjectType,
    -- | ID of the workflow run
    id :: Text,
    -- | Name of the workflow run
    name :: Text,
    -- | URL to view the workflow run
    url :: Text,
    -- | Status of the workflow run
    status :: WorkflowRunStatus,
    -- | Metadata associated with the run
    metadata :: Map Text Value,
    -- | Files associated with the run
    files :: [File],
    -- | When the workflow run was initially created
    initialRunAt :: UTCTime,
    -- | Whether the workflow run has been reviewed
    reviewed :: Bool,
    -- | Outputs from document processors
    outputs :: [ProcessorRun],
    -- | Step runs in the workflow
    stepRuns :: [WorkflowStepRun],
    -- | The workflow this run belongs to
    workflow :: Workflow,
    -- | The batch ID if part of a batch
    batchId :: Maybe Text,
    -- | Reason for failure if failed
    failureReason :: Maybe Text,
    -- | Detailed failure message if failed
    failureMessage :: Maybe Text,
    -- | Who reviewed the run
    reviewedBy :: Maybe Text,
    -- | Note if rejected
    rejectionNote :: Maybe Text,
    -- | When the run was reviewed
    reviewedAt :: Maybe UTCTime,
    -- | When the run started
    startTime :: Maybe UTCTime,
    -- | When the run ended
    endTime :: Maybe UTCTime
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON WorkflowRun where
  parseJSON = Aeson.withObject "WorkflowRun" $ \v -> do
    objectType :: ObjectType <- v Aeson..: "object"
    id <- v Aeson..: "id"
    name <- v Aeson..: "name"
    url <- v Aeson..: "url"
    status <- v Aeson..: "status"
    metadata <- v Aeson..: "metadata"
    files <- v Aeson..: "files"
    initialRunAt <- v Aeson..: "initialRunAt"
    reviewed <- v Aeson..: "reviewed"
    outputs <- v Aeson..: "outputs"
    stepRuns <- v Aeson..: "stepRuns"
    workflow <- v Aeson..: "workflow"
    batchId <- v Aeson..:? "batchId"
    failureReason <- v Aeson..:? "failureReason"
    failureMessage <- v Aeson..:? "failureMessage"
    reviewedBy <- v Aeson..:? "reviewedBy"
    rejectionNote <- v Aeson..:? "rejectionNote"
    reviewedAt <- v Aeson..:? "reviewedAt"
    startTime <- v Aeson..:? "startTime"
    endTime <- v Aeson..:? "endTime"
    pure WorkflowRun {..}

instance ToJSON WorkflowRun where
  toJSON WorkflowRun {..} =
    Aeson.object $
      catMaybes
        [ Just ("object" .= object),
          Just ("id" .= id),
          Just ("name" .= name),
          Just ("url" .= url),
          Just ("status" .= status),
          Just ("metadata" .= metadata),
          Just ("files" .= files),
          Just ("initialRunAt" .= initialRunAt),
          Just ("reviewed" .= reviewed),
          Just ("outputs" .= outputs),
          Just ("stepRuns" .= stepRuns),
          Just ("workflow" .= workflow),
          ("batchId" .=) <$> batchId,
          ("failureReason" .=) <$> failureReason,
          ("failureMessage" .=) <$> failureMessage,
          ("reviewedBy" .=) <$> reviewedBy,
          ("rejectionNote" .=) <$> rejectionNote,
          ("reviewedAt" .=) <$> reviewedAt,
          ("startTime" .=) <$> startTime,
          ("endTime" .=) <$> endTime
        ]

-- | Predetermined output for a processor
data PredeterminedOutput = PredeterminedOutput
  { -- | The ID of the processor that the output is associated with
    processorId :: Text,
    -- | The output that is being overridden
    output :: Value
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | File to process through a workflow
data ExtendFile = ExtendFile
  { -- | The name of the file to be processed
    fileName :: Maybe Text,
    -- | A URL where the file can be downloaded from
    fileUrl :: Maybe Text,
    -- | Extend's internal ID for the file
    fileId :: Maybe Text,
    -- | Optional predetermined outputs to override generated outputs
    outputs :: Maybe [PredeterminedOutput]
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ExtendFile where
  parseJSON = Aeson.withObject "ExtendFile" $ \v -> do
    fileName <- v Aeson..:? "fileName"
    fileUrl <- v Aeson..:? "fileUrl"
    fileId <- v Aeson..:? "fileId"
    outputs <- v Aeson..:? "outputs"
    pure ExtendFile {..}

instance ToJSON ExtendFile where
  toJSON ExtendFile {..} =
    Aeson.object $
      catMaybes
        [ ("fileName" .=) <$> fileName,
          ("fileUrl" .=) <$> fileUrl,
          ("fileId" .=) <$> fileId,
          ("outputs" .=) <$> outputs
        ]

-- | Request to run a workflow
data RunWorkflowRequest = RunWorkflowRequest
  { -- | The ID of the workflow to run
    workflowId :: Text,
    -- | Optional array of files to process through the workflow
    files :: Maybe [ExtendFile],
    -- | Optional array of raw strings to be converted to text files
    rawTexts :: Maybe [Text],
    -- | Optional value (1-100) used to determine relative order of WorkflowRuns when rate limiting is in effect.
    -- Lower values will be prioritized before higher values. Defaults to 50.
    priority :: Maybe Int,
    -- | Optional metadata object that can be assigned to a specific WorkflowRun to help identify it.
    -- It will be returned in the response and webhooks. You can place any arbitrary key : value pairs in this object.
    metadata :: Maybe (Map Text Value)
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON RunWorkflowRequest where
  toJSON RunWorkflowRequest {..} =
    Aeson.object $
      catMaybes
        [ Just ("workflowId" .= workflowId),
          ("files" .=) <$> files,
          ("rawTexts" .=) <$> rawTexts,
          ("priority" .=) <$> priority,
          ("metadata" .=) <$> metadata
        ]

instance FromJSON RunWorkflowRequest where
  parseJSON = Aeson.withObject "RunWorkflowRequest" $ \v -> do
    workflowId <- v Aeson..: "workflowId"
    files <- v Aeson..:? "files"
    rawTexts <- v Aeson..:? "rawTexts"
    priority <- v Aeson..:? "priority"
    metadata <- v Aeson..:? "metadata"
    pure RunWorkflowRequest {..}

-- | Response from getting a workflow
newtype GetWorkflowResponse = GetWorkflowResponse
  { -- | The requested workflow
    workflow :: Workflow
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON GetWorkflowResponse where
  parseJSON = Aeson.withObject "GetWorkflowResponse" $ \v -> do
    workflow <- v Aeson..: "workflow"
    pure GetWorkflowResponse {..}

-- | Response from listing workflows
data ListWorkflowsResponse = ListWorkflowsResponse
  { -- | The workflows
    workflows :: [Workflow],
    -- | Pagination information
    pagination :: Maybe Pagination
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ListWorkflowsResponse where
  parseJSON = Aeson.withObject "ListWorkflowsResponse" $ \v -> do
    workflows <- v Aeson..: "workflows"
    pagination <- v Aeson..:? "pagination"
    pure ListWorkflowsResponse {..}

-- | Response from running a workflow
data RunWorkflowResponse = RunWorkflowResponse
  { -- | The workflow runs that were created
    workflowRuns :: [WorkflowRun]
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON RunWorkflowResponse where
  parseJSON = Aeson.withObject "RunWorkflowResponse" $ \v -> do
    workflowRuns <- v Aeson..: "workflowRuns"
    pure RunWorkflowResponse {..}

-- | Response from getting a workflow run
newtype GetWorkflowRunResponse = GetWorkflowRunResponse
  { -- | The requested workflow run
    workflowRun :: WorkflowRun
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON GetWorkflowRunResponse where
  parseJSON = Aeson.withObject "GetWorkflowRunResponse" $ \v -> do
    workflowRun <- v Aeson..: "workflowRun"
    pure GetWorkflowRunResponse {..}

-- | Response from listing workflow runs
data ListWorkflowRunsResponse = ListWorkflowRunsResponse
  { -- | Whether the request was successful
    success :: Bool,
    -- | The workflow runs
    workflowRuns :: [WorkflowRun],
    -- | The next page token
    nextPageToken :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ListWorkflowRunsResponse where
  parseJSON = Aeson.withObject "ListWorkflowRunsResponse" $ \v -> do
    success <- v Aeson..: "success"
    workflowRuns <- v Aeson..: "workflowRuns"
    nextPageToken <- v Aeson..:? "nextPageToken"
    pure ListWorkflowRunsResponse {..}

-- | Workflows API endpoints
type WorkflowsAPI =
  "workflows"
    :> Capture "workflowId" Text
    :> Header' '[Required, Strict] "Authorization" Text
    :> Header' '[Required, Strict] "x-extend-api-version" Text
    :> Get '[JSON] (SuccessResponse GetWorkflowResponse)
    :<|> "workflows"
    :> Header' '[Required, Strict] "Authorization" Text
    :> Header' '[Required, Strict] "x-extend-api-version" Text
    :> QueryParam "limit" Int
    :> QueryParam "page" Int
    :> Get '[JSON] (SuccessResponse ListWorkflowsResponse)
    :<|> "workflow_runs"
    :> Header' '[Required, Strict] "Authorization" Text
    :> Header' '[Required, Strict] "x-extend-api-version" Text
    :> ReqBody '[JSON] RunWorkflowRequest
    :> Post '[JSON] (SuccessResponse RunWorkflowResponse)
    :<|> "workflow_runs"
    :> Capture "runId" Text
    :> Header' '[Required, Strict] "Authorization" Text
    :> Header' '[Required, Strict] "x-extend-api-version" Text
    :> Get '[JSON] (SuccessResponse GetWorkflowRunResponse)
    :<|> "workflow_runs"
    :> Header' '[Required, Strict] "Authorization" Text
    :> Header' '[Required, Strict] "x-extend-api-version" Text
    :> QueryParam "workflowId" Text
    :> QueryParam "limit" Int
    :> QueryParam "page" Int
    :> Get '[JSON] (SuccessResponse ListWorkflowRunsResponse)

-- | Split the client functions for easier access
workflowsAPI :: Proxy WorkflowsAPI
workflowsAPI = Proxy

getWorkflowClient :: Text -> Text -> Text -> ClientM (SuccessResponse GetWorkflowResponse)
listWorkflowsClient :: Text -> Text -> Maybe Int -> Maybe Int -> ClientM (SuccessResponse ListWorkflowsResponse)
runWorkflowClient :: Text -> Text -> RunWorkflowRequest -> ClientM (SuccessResponse RunWorkflowResponse)
getWorkflowRunClient :: Text -> Text -> Text -> ClientM (SuccessResponse GetWorkflowRunResponse)
listWorkflowRunsClient :: Text -> Text -> Maybe Text -> Maybe Int -> Maybe Int -> ClientM (SuccessResponse ListWorkflowRunsResponse)
getWorkflowClient :<|> listWorkflowsClient :<|> runWorkflowClient :<|> getWorkflowRunClient :<|> listWorkflowRunsClient = client workflowsAPI

-- | Get a workflow
getWorkflow ::
  ApiToken ->
  ApiVersion ->
  -- | Workflow ID
  Text ->
  ClientM (SuccessResponse GetWorkflowResponse)
getWorkflow (ApiToken token) (ApiVersion version) workflowId =
  getWorkflowClient workflowId ("Bearer " <> token) version

-- | List workflows
listWorkflows ::
  ApiToken ->
  ApiVersion ->
  -- | Limit
  Maybe Int ->
  -- | Page
  Maybe Int ->
  ClientM (SuccessResponse ListWorkflowsResponse)
listWorkflows (ApiToken token) (ApiVersion version) = listWorkflowsClient ("Bearer " <> token) version

-- | Run a workflow
runWorkflow ::
  ApiToken ->
  ApiVersion ->
  RunWorkflowRequest ->
  ClientM (SuccessResponse RunWorkflowResponse)
runWorkflow (ApiToken token) (ApiVersion version) = runWorkflowClient ("Bearer " <> token) version

-- | Get a workflow run
getWorkflowRun ::
  ApiToken ->
  ApiVersion ->
  -- | Run ID
  Text ->
  ClientM (SuccessResponse GetWorkflowRunResponse)
getWorkflowRun (ApiToken token) (ApiVersion version) runId =
  getWorkflowRunClient runId ("Bearer " <> token) version

-- | List workflow runs
listWorkflowRuns ::
  ApiToken ->
  ApiVersion ->
  -- | Workflow ID
  Maybe Text ->
  -- | Limit
  Maybe Int ->
  -- | Page
  Maybe Int ->
  ClientM (SuccessResponse ListWorkflowRunsResponse)
listWorkflowRuns (ApiToken token) (ApiVersion version) = listWorkflowRunsClient ("Bearer " <> token) version