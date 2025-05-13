{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

-- | Workflow operations for the Extend API
module Extend.V1.Workflows
  ( -- * Types
    Workflow (..),
    CreatedWorkflow (..),
    WorkflowStep (..),
    WorkflowStepRun (..),
    ValidationRule (..),
    StepRunOutput (..),
    WorkflowRun (..),
    CreatedWorkflowRun (..),
    WorkflowRunStatus (..),
    ExtendFile (..),
    PredeterminedOutput (..),
    GetWorkflowResponse (..),
    RunWorkflowRequest (..),
    RunWorkflowResponse (..),
    GetWorkflowRunResponse (..),
    ListWorkflowRunsResponse (..),

    -- * API
    WorkflowsAPI,
    getWorkflow,
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
    workflowStepObject :: ObjectType,
    -- | ID of the step
    workflowStepId :: Text,
    -- | Name of the step
    workflowStepName :: Text,
    -- | Type of the step
    workflowStepType :: Text
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
        { workflowStepObject = objectType,
          workflowStepId = id,
          workflowStepName = name,
          workflowStepType = type_
        }

instance ToJSON WorkflowStep where
  toJSON WorkflowStep {..} =
    Aeson.object
      [ "object" .= workflowStepObject,
        "id" .= workflowStepId,
        "name" .= workflowStepName,
        "type" .= workflowStepType
      ]

-- | Validation rule for a workflow step
data ValidationRule = ValidationRule
  { -- | Name of the rule
    validationRuleName :: Text,
    -- | Whether the rule is valid
    validationRuleValid :: Bool,
    -- | Array of valid values
    validationRuleValidArray :: Maybe [Text],
    -- | Reason for failure
    validationRuleFailureReason :: Maybe Text,
    -- | Error message
    validationRuleError :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ValidationRule where
  parseJSON = Aeson.withObject "ValidationRule" $ \v -> do
    name <- v Aeson..: "name"
    valid <- v Aeson..: "valid"
    validArray <- v Aeson..:? "validArray"
    failureReason <- v Aeson..:? "failureReason"
    error <- v Aeson..:? "error"
    pure
      ValidationRule
        { validationRuleName = name,
          validationRuleValid = valid,
          validationRuleValidArray = validArray,
          validationRuleFailureReason = failureReason,
          validationRuleError = error
        }

instance ToJSON ValidationRule where
  toJSON ValidationRule {..} =
    Aeson.object $
      catMaybes
        [ Just ("name" .= validationRuleName),
          Just ("valid" .= validationRuleValid),
          ("validArray" .=) <$> validationRuleValidArray,
          ("failureReason" .=) <$> validationRuleFailureReason,
          ("error" .=) <$> validationRuleError
        ]

-- | Output of a workflow step run
data StepRunOutput = StepRunOutput
  { -- | Validation rules
    stepRunOutputRules :: Maybe [ValidationRule]
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON StepRunOutput where
  parseJSON = Aeson.withObject "StepRunOutput" $ \v -> do
    rules <- v Aeson..:? "rules"
    pure StepRunOutput {stepRunOutputRules = rules}

instance ToJSON StepRunOutput where
  toJSON StepRunOutput {..} =
    Aeson.object $
      catMaybes
        [ ("rules" .=) <$> stepRunOutputRules
        ]

-- | A workflow step run
data WorkflowStepRun = WorkflowStepRun
  { -- | Type of the object
    workflowStepRunObject :: ObjectType,
    -- | ID of the step run
    workflowStepRunId :: Text,
    -- | Status of the step run
    workflowStepRunStatus :: Text,
    -- | The step that was run
    workflowStepRunStep :: WorkflowStep,
    -- | Output from the step
    workflowStepRunOutput :: Maybe StepRunOutput
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
        { workflowStepRunObject = objectType,
          workflowStepRunId = id,
          workflowStepRunStatus = status,
          workflowStepRunStep = step,
          workflowStepRunOutput = output
        }

instance ToJSON WorkflowStepRun where
  toJSON WorkflowStepRun {..} =
    Aeson.object $
      catMaybes
        [ Just ("object" .= workflowStepRunObject),
          Just ("id" .= workflowStepRunId),
          Just ("status" .= workflowStepRunStatus),
          Just ("step" .= workflowStepRunStep),
          ("output" .=) <$> workflowStepRunOutput
        ]

-- | A workflow
data Workflow = Workflow
  { -- | Type of the object
    workflowObject :: ObjectType,
    -- | ID of the workflow
    workflowId :: Text,
    -- | Name of the workflow
    workflowName :: Text,
    -- | Description of the workflow
    workflowDescription :: Maybe Text,
    -- | Version of the workflow
    workflowVersion :: Text,
    -- | When the workflow was created
    workflowCreatedAt :: UTCTime,
    -- | When the workflow was last updated
    workflowUpdatedAt :: UTCTime
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
        { workflowObject = objectType,
          workflowId = id,
          workflowName = name,
          workflowDescription = description,
          workflowVersion = version,
          workflowCreatedAt = createdAt,
          workflowUpdatedAt = updatedAt
        }

instance ToJSON Workflow where
  toJSON Workflow {..} =
    Aeson.object $
      catMaybes
        [ Just ("object" .= workflowObject),
          Just ("id" .= workflowId),
          Just ("name" .= workflowName),
          ("description" .=) <$> workflowDescription,
          Just ("version" .= workflowVersion),
          Just ("createdAt" .= workflowCreatedAt),
          Just ("updatedAt" .= workflowUpdatedAt)
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
    workflowRunObject :: Maybe ObjectType,
    -- | ID of the workflow run
    workflowRunId :: Text,
    -- | Name of the workflow run
    workflowRunName :: Maybe Text,
    -- | Dashboard URL for the workflow run
    workflowRunUrl :: Maybe Text,
    -- | Status of the workflow run
    workflowRunStatus :: WorkflowRunStatus,
    -- | Metadata for the workflow run
    workflowRunMetadata :: Maybe (Map Text Value),
    -- | Files associated with the workflow run
    workflowRunFiles :: Maybe [File],
    -- | The ID of the workflow that was run
    workflowRunWorkflowId :: Text,
    -- | The name of the workflow that was run
    workflowRunWorkflowName :: Text,
    -- | The ID of the workflow version that was run
    workflowRunWorkflowVersionId :: Text,
    -- | When the workflow run was created
    workflowRunCreatedAt :: UTCTime,
    -- | When the workflow run was last updated
    workflowRunUpdatedAt :: UTCTime,
    -- | When the workflow run was initially created
    workflowRunInitialRunAt :: Maybe UTCTime,
    -- | Whether the workflow run has been reviewed
    workflowRunReviewed :: Bool,
    -- | Who reviewed the workflow run
    workflowRunReviewedBy :: Maybe Text,
    -- | When the workflow run was reviewed
    workflowRunReviewedAt :: Maybe UTCTime,
    -- | When the run started
    workflowRunStartTime :: Maybe UTCTime,
    -- | When the run ended
    workflowRunEndTime :: Maybe UTCTime,
    -- | Processor output results
    workflowRunOutputs :: Maybe [ProcessorRun],
    -- | Workflow step runs
    workflowRunStepRuns :: Maybe [WorkflowStepRun],
    -- | The associated workflow
    workflowRunWorkflow :: Maybe Workflow,
    -- | The batch ID if part of a batch
    workflowRunBatchId :: Maybe Text,
    -- | Reason for failure
    workflowRunFailureReason :: Maybe Text,
    -- | Detailed failure message
    workflowRunFailureMessage :: Maybe Text,
    -- | Note if rejected
    workflowRunRejectionNote :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON WorkflowRun where
  parseJSON = Aeson.withObject "WorkflowRun" $ \v -> do
    objectType :: Maybe ObjectType <- v Aeson..:? "object"
    id <- v Aeson..: "id"
    name <- v Aeson..:? "name"
    url <- v Aeson..:? "url"
    status <- v Aeson..: "status"
    metadata <- v Aeson..:? "metadata"
    files <- v Aeson..:? "files"
    workflowId <- v Aeson..: "workflowId"
    workflowName <- v Aeson..: "workflowName"
    workflowVersionId <- v Aeson..: "workflowVersionId"
    createdAt <- v Aeson..: "createdAt"
    updatedAt <- v Aeson..: "updatedAt"
    initialRunAt <- v Aeson..:? "initialRunAt"
    reviewed <- v Aeson..: "reviewed"
    reviewedBy <- v Aeson..:? "reviewedBy"
    reviewedAt <- v Aeson..:? "reviewedAt"
    startTime <- v Aeson..:? "startTime"
    endTime <- v Aeson..:? "endTime"
    outputs <- v Aeson..:? "outputs"
    stepRuns <- v Aeson..:? "stepRuns"
    workflow <- v Aeson..:? "workflow"
    batchId <- v Aeson..:? "batchId"
    failureReason <- v Aeson..:? "failureReason"
    failureMessage <- v Aeson..:? "failureMessage"
    rejectionNote <- v Aeson..:? "rejectionNote"
    pure
      WorkflowRun
        { workflowRunObject = objectType,
          workflowRunId = id,
          workflowRunName = name,
          workflowRunUrl = url,
          workflowRunStatus = status,
          workflowRunMetadata = metadata,
          workflowRunFiles = files,
          workflowRunWorkflowId = workflowId,
          workflowRunWorkflowName = workflowName,
          workflowRunWorkflowVersionId = workflowVersionId,
          workflowRunCreatedAt = createdAt,
          workflowRunUpdatedAt = updatedAt,
          workflowRunInitialRunAt = initialRunAt,
          workflowRunReviewed = reviewed,
          workflowRunReviewedBy = reviewedBy,
          workflowRunReviewedAt = reviewedAt,
          workflowRunStartTime = startTime,
          workflowRunEndTime = endTime,
          workflowRunOutputs = outputs,
          workflowRunStepRuns = stepRuns,
          workflowRunWorkflow = workflow,
          workflowRunBatchId = batchId,
          workflowRunFailureReason = failureReason,
          workflowRunFailureMessage = failureMessage,
          workflowRunRejectionNote = rejectionNote
        }

instance ToJSON WorkflowRun where
  toJSON WorkflowRun {..} =
    Aeson.object $
      catMaybes
        [ ("object" .=) <$> workflowRunObject,
          Just ("id" .= workflowRunId),
          ("name" .=) <$> workflowRunName,
          ("url" .=) <$> workflowRunUrl,
          Just ("status" .= workflowRunStatus),
          ("metadata" .=) <$> workflowRunMetadata,
          ("files" .=) <$> workflowRunFiles,
          Just ("workflowId" .= workflowRunWorkflowId),
          Just ("workflowName" .= workflowRunWorkflowName),
          Just ("workflowVersionId" .= workflowRunWorkflowVersionId),
          Just ("createdAt" .= workflowRunCreatedAt),
          Just ("updatedAt" .= workflowRunUpdatedAt),
          Just ("reviewed" .= workflowRunReviewed),
          ("initialRunAt" .=) <$> workflowRunInitialRunAt,
          ("reviewedBy" .=) <$> workflowRunReviewedBy,
          ("reviewedAt" .=) <$> workflowRunReviewedAt,
          ("startTime" .=) <$> workflowRunStartTime,
          ("endTime" .=) <$> workflowRunEndTime,
          ("outputs" .=) <$> workflowRunOutputs,
          ("stepRuns" .=) <$> workflowRunStepRuns,
          ("workflow" .=) <$> workflowRunWorkflow,
          ("batchId" .=) <$> workflowRunBatchId,
          ("failureReason" .=) <$> workflowRunFailureReason,
          ("failureMessage" .=) <$> workflowRunFailureMessage,
          ("rejectionNote" .=) <$> workflowRunRejectionNote
        ]

-- | Predetermined output for a processor
data PredeterminedOutput = PredeterminedOutput
  { -- | The ID of the processor that the output is associated with
    predeterminedOutputProcessorId :: Text,
    -- | The output that is being overridden
    predeterminedOutputOutput :: Value
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON PredeterminedOutput where
  parseJSON = Aeson.withObject "PredeterminedOutput" $ \v -> do
    processorId <- v Aeson..: "processorId"
    output <- v Aeson..: "output"
    pure
      PredeterminedOutput
        { predeterminedOutputProcessorId = processorId,
          predeterminedOutputOutput = output
        }

instance ToJSON PredeterminedOutput where
  toJSON PredeterminedOutput {..} =
    Aeson.object
      [ "processorId" .= predeterminedOutputProcessorId,
        "output" .= predeterminedOutputOutput
      ]

-- | File to process through a workflow
data ExtendFile = ExtendFile
  { -- | The name of the file to be processed
    extendFileFileName :: Maybe Text,
    -- | A URL where the file can be downloaded from
    extendFileFileUrl :: Maybe Text,
    -- | Extend's internal ID for the file
    extendFileFileId :: Maybe Text,
    -- | Optional predetermined outputs to override generated outputs
    extendFileOutputs :: Maybe [PredeterminedOutput]
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ExtendFile where
  parseJSON = Aeson.withObject "ExtendFile" $ \v -> do
    fileName <- v Aeson..:? "fileName"
    fileUrl <- v Aeson..:? "fileUrl"
    fileId <- v Aeson..:? "fileId"
    outputs <- v Aeson..:? "outputs"
    pure
      ExtendFile
        { extendFileFileName = fileName,
          extendFileFileUrl = fileUrl,
          extendFileFileId = fileId,
          extendFileOutputs = outputs
        }

instance ToJSON ExtendFile where
  toJSON ExtendFile {..} =
    Aeson.object $
      catMaybes
        [ ("fileName" .=) <$> extendFileFileName,
          ("fileUrl" .=) <$> extendFileFileUrl,
          ("fileId" .=) <$> extendFileFileId,
          ("outputs" .=) <$> extendFileOutputs
        ]

-- | Request to run a workflow
data RunWorkflowRequest = RunWorkflowRequest
  { -- | The ID of the workflow to run
    runWorkflowRequestWorkflowId :: Text,
    -- | Optional array of files to process through the workflow
    runWorkflowRequestFiles :: Maybe [ExtendFile],
    -- | Optional array of raw strings to be converted to text files
    runWorkflowRequestRawTexts :: Maybe [Text],
    -- | Optional value (1-100) used to determine relative order of WorkflowRuns when rate limiting is in effect.
    -- Lower values will be prioritized before higher values. Defaults to 50.
    runWorkflowRequestPriority :: Maybe Int,
    -- | Optional metadata object that can be assigned to a specific WorkflowRun to help identify it.
    -- It will be returned in the response and webhooks. You can place any arbitrary key : value pairs in this object.
    runWorkflowRequestMetadata :: Maybe (Map Text Value),
    -- | Optional version of the workflow to run
    runWorkflowRequestVersion :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON RunWorkflowRequest where
  toJSON RunWorkflowRequest {..} =
    Aeson.object $
      catMaybes
        [ Just ("workflowId" .= runWorkflowRequestWorkflowId),
          ("files" .=) <$> runWorkflowRequestFiles,
          ("rawTexts" .=) <$> runWorkflowRequestRawTexts,
          ("priority" .=) <$> runWorkflowRequestPriority,
          ("metadata" .=) <$> runWorkflowRequestMetadata,
          ("version" .=) <$> runWorkflowRequestVersion
        ]

instance FromJSON RunWorkflowRequest where
  parseJSON = Aeson.withObject "RunWorkflowRequest" $ \v -> do
    workflowId <- v Aeson..: "workflowId"
    files <- v Aeson..:? "files"
    rawTexts <- v Aeson..:? "rawTexts"
    priority <- v Aeson..:? "priority"
    metadata <- v Aeson..:? "metadata"
    version <- v Aeson..:? "version"
    pure
      RunWorkflowRequest
        { runWorkflowRequestWorkflowId = workflowId,
          runWorkflowRequestFiles = files,
          runWorkflowRequestRawTexts = rawTexts,
          runWorkflowRequestPriority = priority,
          runWorkflowRequestMetadata = metadata,
          runWorkflowRequestVersion = version
        }

-- | Response from getting a workflow
data GetWorkflowResponse = GetWorkflowResponse
  { -- | Whether the request was successful
    getWorkflowResponseSuccess :: Bool,
    -- | The requested workflow
    getWorkflowResponseWorkflow :: Workflow
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON GetWorkflowResponse where
  parseJSON = Aeson.withObject "GetWorkflowResponse" $ \v -> do
    success <- v Aeson..: "success"
    workflow <- v Aeson..: "workflow"
    pure
      GetWorkflowResponse
        { getWorkflowResponseSuccess = success,
          getWorkflowResponseWorkflow = workflow
        }

instance ToJSON GetWorkflowResponse where
  toJSON GetWorkflowResponse {..} =
    Aeson.object
      [ "success" .= getWorkflowResponseSuccess,
        "workflow" .= getWorkflowResponseWorkflow
      ]

-- | Response from listing workflows
data ListWorkflowsResponse = ListWorkflowsResponse
  { -- | The workflows
    listWorkflowsResponseWorkflows :: [Workflow],
    -- | Pagination information
    listWorkflowsResponsePagination :: Maybe Pagination
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ListWorkflowsResponse where
  parseJSON = Aeson.withObject "ListWorkflowsResponse" $ \v -> do
    workflows <- v Aeson..: "workflows"
    pagination <- v Aeson..:? "pagination"
    pure
      ListWorkflowsResponse
        { listWorkflowsResponseWorkflows = workflows,
          listWorkflowsResponsePagination = pagination
        }

instance ToJSON ListWorkflowsResponse where
  toJSON ListWorkflowsResponse {..} =
    Aeson.object $
      catMaybes
        [ Just ("workflows" .= listWorkflowsResponseWorkflows),
          ("pagination" .=) <$> listWorkflowsResponsePagination
        ]

-- | A simplified workflow returned in the run workflow response
data CreatedWorkflow = CreatedWorkflow
  { -- | Type of the object
    createdWorkflowObject :: ObjectType,
    -- | ID of the workflow
    createdWorkflowId :: Text,
    -- | Version of the workflow
    createdWorkflowVersion :: Text,
    -- | Name of the workflow
    createdWorkflowName :: Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON CreatedWorkflow where
  parseJSON = Aeson.withObject "CreatedWorkflow" $ \v -> do
    objectType <- v Aeson..: "object"
    id <- v Aeson..: "id"
    version <- v Aeson..: "version"
    name <- v Aeson..: "name"
    pure
      CreatedWorkflow
        { createdWorkflowObject = objectType,
          createdWorkflowId = id,
          createdWorkflowVersion = version,
          createdWorkflowName = name
        }

instance ToJSON CreatedWorkflow where
  toJSON CreatedWorkflow {..} =
    Aeson.object
      [ "object" .= createdWorkflowObject,
        "id" .= createdWorkflowId,
        "version" .= createdWorkflowVersion,
        "name" .= createdWorkflowName
      ]

-- | A workflow run returned from the run workflow creation API
data CreatedWorkflowRun = CreatedWorkflowRun
  { -- | Type of the object
    createdWorkflowRunObject :: ObjectType,
    -- | ID of the workflow run
    createdWorkflowRunId :: Text,
    -- | Name of the workflow run
    createdWorkflowRunName :: Maybe Text,
    -- | Dashboard URL for the workflow run
    createdWorkflowRunUrl :: Maybe Text,
    -- | Status of the workflow run
    createdWorkflowRunStatus :: WorkflowRunStatus,
    -- | Metadata for the workflow run
    createdWorkflowRunMetadata :: Maybe (Map Text Value),
    -- | Files associated with the workflow run
    createdWorkflowRunFiles :: Maybe [File],
    -- | When the workflow run was initially created
    createdWorkflowRunInitialRunAt :: Maybe UTCTime,
    -- | Whether the workflow run has been reviewed
    createdWorkflowRunReviewed :: Bool,
    -- | Processor output results
    createdWorkflowRunOutputs :: Maybe [ProcessorRun],
    -- | Workflow step runs
    createdWorkflowRunStepRuns :: Maybe [WorkflowStepRun],
    -- | The associated workflow
    createdWorkflowRunWorkflow :: Maybe CreatedWorkflow,
    -- | The batch ID if part of a batch
    createdWorkflowRunBatchId :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON CreatedWorkflowRun where
  parseJSON = Aeson.withObject "CreatedWorkflowRun" $ \v -> do
    objectType <- v Aeson..: "object"
    id <- v Aeson..: "id"
    name <- v Aeson..:? "name"
    url <- v Aeson..:? "url"
    status <- v Aeson..: "status"
    metadata <- v Aeson..:? "metadata"
    files <- v Aeson..:? "files"
    initialRunAt <- v Aeson..:? "initialRunAt"
    reviewed <- v Aeson..: "reviewed"
    outputs <- v Aeson..:? "outputs"
    stepRuns <- v Aeson..:? "stepRuns"
    workflow <- v Aeson..:? "workflow"
    batchId <- v Aeson..:? "batchId"
    pure
      CreatedWorkflowRun
        { createdWorkflowRunObject = objectType,
          createdWorkflowRunId = id,
          createdWorkflowRunName = name,
          createdWorkflowRunUrl = url,
          createdWorkflowRunStatus = status,
          createdWorkflowRunMetadata = metadata,
          createdWorkflowRunFiles = files,
          createdWorkflowRunInitialRunAt = initialRunAt,
          createdWorkflowRunReviewed = reviewed,
          createdWorkflowRunOutputs = outputs,
          createdWorkflowRunStepRuns = stepRuns,
          createdWorkflowRunWorkflow = workflow,
          createdWorkflowRunBatchId = batchId
        }

instance ToJSON CreatedWorkflowRun where
  toJSON CreatedWorkflowRun {..} =
    Aeson.object $
      catMaybes
        [ Just ("object" .= createdWorkflowRunObject),
          Just ("id" .= createdWorkflowRunId),
          ("name" .=) <$> createdWorkflowRunName,
          ("url" .=) <$> createdWorkflowRunUrl,
          Just ("status" .= createdWorkflowRunStatus),
          ("metadata" .=) <$> createdWorkflowRunMetadata,
          ("files" .=) <$> createdWorkflowRunFiles,
          ("initialRunAt" .=) <$> createdWorkflowRunInitialRunAt,
          Just ("reviewed" .= createdWorkflowRunReviewed),
          ("outputs" .=) <$> createdWorkflowRunOutputs,
          ("stepRuns" .=) <$> createdWorkflowRunStepRuns,
          ("workflow" .=) <$> createdWorkflowRunWorkflow,
          ("batchId" .=) <$> createdWorkflowRunBatchId
        ]

-- | Response from running a workflow
data RunWorkflowResponse = RunWorkflowResponse
  { -- | Whether the request was successful
    runWorkflowResponseSuccess :: Bool,
    -- | The workflow runs that were created
    runWorkflowResponseWorkflowRuns :: [CreatedWorkflowRun]
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON RunWorkflowResponse where
  parseJSON = Aeson.withObject "RunWorkflowResponse" $ \v -> do
    success <- v Aeson..: "success"
    workflowRuns <- v Aeson..: "workflowRuns"
    pure
      RunWorkflowResponse
        { runWorkflowResponseSuccess = success,
          runWorkflowResponseWorkflowRuns = workflowRuns
        }

instance ToJSON RunWorkflowResponse where
  toJSON RunWorkflowResponse {..} =
    Aeson.object
      [ "success" .= runWorkflowResponseSuccess,
        "workflowRuns" .= runWorkflowResponseWorkflowRuns
      ]

-- | Response from getting a workflow run
data GetWorkflowRunResponse = GetWorkflowRunResponse
  { -- | Whether the request was successful
    getWorkflowRunResponseSuccess :: Bool,
    -- | The requested workflow run
    getWorkflowRunResponseWorkflowRun :: WorkflowRun
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON GetWorkflowRunResponse where
  parseJSON = Aeson.withObject "GetWorkflowRunResponse" $ \v -> do
    success <- v Aeson..: "success"
    workflowRun <- v Aeson..: "workflowRun"
    pure
      GetWorkflowRunResponse
        { getWorkflowRunResponseSuccess = success,
          getWorkflowRunResponseWorkflowRun = workflowRun
        }

instance ToJSON GetWorkflowRunResponse where
  toJSON GetWorkflowRunResponse {..} =
    Aeson.object
      [ "success" .= getWorkflowRunResponseSuccess,
        "workflowRun" .= getWorkflowRunResponseWorkflowRun
      ]

-- | Response from listing workflow runs
data ListWorkflowRunsResponse = ListWorkflowRunsResponse
  { -- | Whether the request was successful
    listWorkflowRunsResponseSuccess :: Bool,
    -- | The workflow runs
    listWorkflowRunsResponseWorkflowRuns :: [WorkflowRun],
    -- | The next page token
    listWorkflowRunsResponseNextPageToken :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ListWorkflowRunsResponse where
  parseJSON = Aeson.withObject "ListWorkflowRunsResponse" $ \v -> do
    success <- v Aeson..: "success"
    workflowRuns <- v Aeson..: "workflowRuns"
    nextPageToken <- v Aeson..:? "nextPageToken"
    pure
      ListWorkflowRunsResponse
        { listWorkflowRunsResponseSuccess = success,
          listWorkflowRunsResponseWorkflowRuns = workflowRuns,
          listWorkflowRunsResponseNextPageToken = nextPageToken
        }

instance ToJSON ListWorkflowRunsResponse where
  toJSON ListWorkflowRunsResponse {..} =
    Aeson.object $
      catMaybes
        [ Just ("success" .= listWorkflowRunsResponseSuccess),
          Just ("workflowRuns" .= listWorkflowRunsResponseWorkflowRuns),
          ("nextPageToken" .=) <$> listWorkflowRunsResponseNextPageToken
        ]

-- | Workflows API endpoints
type WorkflowsAPI =
  "workflows"
    :> Capture "workflowId" Text
    :> Header' '[Required, Strict] "Authorization" Text
    :> Header' '[Required, Strict] "x-extend-api-version" Text
    :> Get '[JSON] GetWorkflowResponse
    :<|> "workflow_runs"
    :> Header' '[Required, Strict] "Authorization" Text
    :> Header' '[Required, Strict] "x-extend-api-version" Text
    :> ReqBody '[JSON] RunWorkflowRequest
    :> Post '[JSON] RunWorkflowResponse
    :<|> "workflow_runs"
    :> Capture "runId" Text
    :> Header' '[Required, Strict] "Authorization" Text
    :> Header' '[Required, Strict] "x-extend-api-version" Text
    :> Get '[JSON] GetWorkflowRunResponse
    :<|> "workflow_runs"
    :> Header' '[Required, Strict] "Authorization" Text
    :> Header' '[Required, Strict] "x-extend-api-version" Text
    :> QueryParam "workflowId" Text
    :> QueryParam "status" Text
    :> QueryParam "fileNameContains" Text
    :> QueryParam "sortBy" Text
    :> QueryParam "sortDir" Text
    :> QueryParam "nextPageToken" Text
    :> QueryParam "maxPageSize" Int
    :> Get '[JSON] ListWorkflowRunsResponse

-- | Split the client functions for easier access
workflowsAPI :: Proxy WorkflowsAPI
workflowsAPI = Proxy

getWorkflowClient :: Text -> Text -> Text -> ClientM GetWorkflowResponse
runWorkflowClient :: Text -> Text -> RunWorkflowRequest -> ClientM RunWorkflowResponse
getWorkflowRunClient :: Text -> Text -> Text -> ClientM GetWorkflowRunResponse
listWorkflowRunsClient :: Text -> Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int -> ClientM ListWorkflowRunsResponse
getWorkflowClient :<|> runWorkflowClient :<|> getWorkflowRunClient :<|> listWorkflowRunsClient = client workflowsAPI

-- | Get a workflow
getWorkflow ::
  ApiToken ->
  ApiVersion ->
  -- | Workflow ID
  Text ->
  ClientM GetWorkflowResponse
getWorkflow (ApiToken token) (ApiVersion version) workflowId =
  getWorkflowClient workflowId ("Bearer " <> token) version

-- | Run a workflow
runWorkflow ::
  ApiToken ->
  ApiVersion ->
  RunWorkflowRequest ->
  ClientM RunWorkflowResponse
runWorkflow (ApiToken token) (ApiVersion version) = runWorkflowClient ("Bearer " <> token) version

-- | Get a workflow run
getWorkflowRun ::
  ApiToken ->
  ApiVersion ->
  -- | Run ID
  Text ->
  ClientM GetWorkflowRunResponse
getWorkflowRun (ApiToken token) (ApiVersion version) runId =
  getWorkflowRunClient runId ("Bearer " <> token) version

-- | List workflow runs
listWorkflowRuns ::
  ApiToken ->
  ApiVersion ->
  -- | Workflow ID
  Maybe Text ->
  -- | Status (PENDING, PROCESSING, NEEDS_REVIEW, REJECTED, PROCESSED, FAILED)
  Maybe Text ->
  -- | Filter by file name containing this string
  Maybe Text ->
  -- | Sort by field (updatedAt, createdAt)
  Maybe Text ->
  -- | Sort direction (asc, desc)
  Maybe Text ->
  -- | Next page token
  Maybe Text ->
  -- | Maximum page size (1-1000, default 10)
  Maybe Int ->
  ClientM ListWorkflowRunsResponse
listWorkflowRuns (ApiToken token) (ApiVersion version) = listWorkflowRunsClient ("Bearer " <> token) version