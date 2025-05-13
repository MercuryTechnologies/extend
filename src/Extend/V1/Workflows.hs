{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

-- | Workflow operations for the Extend API
module Extend.V1.Workflows
  ( -- * Types
    Workflow (..),
    WorkflowSummary (..),
    CreatedWorkflow (..),
    WorkflowStep (..),
    WorkflowStepRun (..),
    ValidationRule (..),
    StepRunOutput (..),
    WorkflowRun (..),
    WorkflowRunSummary (..),
    CreatedWorkflowRun (..),
    WorkflowRunStatus (..),
    ExtendFile (..),
    PredeterminedOutput (..),
    GetWorkflowResponse (..),
    RunWorkflowRequest (..),
    RunWorkflowResponse (..),
    GetWorkflowRunResponse (..),
    ListWorkflowRunsResponse (..),
    DocumentProcessorRun (..),
    MergedProcessor (..),

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
    stepId <- v Aeson..: "id"
    name <- v Aeson..: "name"
    type_ <- v Aeson..: "type"
    pure
      WorkflowStep
        { workflowStepObject = objectType,
          workflowStepId = stepId,
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
    validationError <- v Aeson..:? "error"
    pure
      ValidationRule
        { validationRuleName = name,
          validationRuleValid = valid,
          validationRuleValidArray = validArray,
          validationRuleFailureReason = failureReason,
          validationRuleError = validationError
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
    stepRunId <- v Aeson..: "id"
    status <- v Aeson..: "status"
    step <- v Aeson..: "step"
    output <- v Aeson..:? "output"
    pure
      WorkflowStepRun
        { workflowStepRunObject = objectType,
          workflowStepRunId = stepRunId,
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
    workflowId <- v Aeson..: "id"
    name <- v Aeson..: "name"
    description <- v Aeson..:? "description"
    version <- v Aeson..: "version"
    createdAt <- v Aeson..: "createdAt"
    updatedAt <- v Aeson..: "updatedAt"
    pure
      Workflow
        { workflowObject = objectType,
          workflowId = workflowId,
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

-- | A simplified workflow summary returned in workflow runs
data WorkflowSummary = WorkflowSummary
  { -- | Type of the object
    workflowSummaryObject :: ObjectType,
    -- | ID of the workflow
    workflowSummaryId :: Text,
    -- | Version of the workflow
    workflowSummaryVersion :: Text,
    -- | Name of the workflow
    workflowSummaryName :: Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON WorkflowSummary where
  parseJSON = Aeson.withObject "WorkflowSummary" $ \v -> do
    objectType <- v Aeson..: "object"
    workflowSummaryId <- v Aeson..: "id"
    version <- v Aeson..: "version"
    name <- v Aeson..: "name"
    pure
      WorkflowSummary
        { workflowSummaryObject = objectType,
          workflowSummaryId = workflowSummaryId,
          workflowSummaryVersion = version,
          workflowSummaryName = name
        }

instance ToJSON WorkflowSummary where
  toJSON WorkflowSummary {..} =
    Aeson.object
      [ "object" .= workflowSummaryObject,
        "id" .= workflowSummaryId,
        "version" .= workflowSummaryVersion,
        "name" .= workflowSummaryName
      ]

-- | A merged processor reference in document processor run outputs
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

-- | A document processor run in workflow outputs
data DocumentProcessorRun = DocumentProcessorRun
  { -- | Type of the object
    documentProcessorRunObject :: ObjectType,
    -- | ID of the processor run
    documentProcessorRunId :: Text,
    -- | ID of the processor
    documentProcessorRunProcessorId :: Text,
    -- | ID of the processor version
    documentProcessorRunProcessorVersionId :: Text,
    -- | Name of the processor
    documentProcessorRunProcessorName :: Text,
    -- | Status of the processor run
    documentProcessorRunStatus :: Text,
    -- | Output from the processor
    documentProcessorRunOutput :: Value,
    -- | Whether the output has been reviewed
    documentProcessorRunReviewed :: Bool,
    -- | Whether the output has been edited
    documentProcessorRunEdited :: Bool,
    -- | Edits made to the output
    documentProcessorRunEdits :: Value,
    -- | Type of processor
    documentProcessorRunType :: Maybe Text,
    -- | Processor configuration
    documentProcessorRunConfig :: Maybe Value,
    -- | Files processed
    documentProcessorRunFiles :: Maybe [File],
    -- | Merged processors
    documentProcessorRunMergedProcessors :: Maybe [MergedProcessor],
    -- | Dashboard URL for the processor run
    documentProcessorRunUrl :: Maybe Text,
    -- | Reason for failure if failed
    documentProcessorRunFailureReason :: Maybe Text,
    -- | Detailed failure message if failed
    documentProcessorRunFailureMessage :: Maybe Text,
    -- | Metadata for the processor run
    documentProcessorRunMetadata :: Maybe Value,
    -- | Initial output from the processor
    documentProcessorRunInitialOutput :: Maybe Value,
    -- | Reviewed output from the processor
    documentProcessorRunReviewedOutput :: Maybe Value
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON DocumentProcessorRun where
  parseJSON = Aeson.withObject "DocumentProcessorRun" $ \v -> do
    objectType <- v Aeson..: "object"
    documentProcessorRunId <- v Aeson..: "id"
    processorId <- v Aeson..: "processorId"
    processorVersionId <- v Aeson..: "processorVersionId"
    processorName <- v Aeson..: "processorName"
    status <- v Aeson..: "status"
    output <- v Aeson..: "output"
    reviewed <- v Aeson..: "reviewed"
    edited <- v Aeson..: "edited"
    edits <- v Aeson..: "edits"
    type_ <- v Aeson..:? "type"
    config <- v Aeson..:? "config"
    files <- v Aeson..:? "files"
    mergedProcessors <- v Aeson..:? "mergedProcessors"
    url <- v Aeson..:? "url"
    failureReason <- v Aeson..:? "failureReason"
    failureMessage <- v Aeson..:? "failureMessage"
    metadata <- v Aeson..:? "metadata"
    initialOutput <- v Aeson..:? "initialOutput"
    reviewedOutput <- v Aeson..:? "reviewedOutput"
    pure
      DocumentProcessorRun
        { documentProcessorRunObject = objectType,
          documentProcessorRunId = documentProcessorRunId,
          documentProcessorRunProcessorId = processorId,
          documentProcessorRunProcessorVersionId = processorVersionId,
          documentProcessorRunProcessorName = processorName,
          documentProcessorRunStatus = status,
          documentProcessorRunOutput = output,
          documentProcessorRunReviewed = reviewed,
          documentProcessorRunEdited = edited,
          documentProcessorRunEdits = edits,
          documentProcessorRunType = type_,
          documentProcessorRunConfig = config,
          documentProcessorRunFiles = files,
          documentProcessorRunMergedProcessors = mergedProcessors,
          documentProcessorRunUrl = url,
          documentProcessorRunFailureReason = failureReason,
          documentProcessorRunFailureMessage = failureMessage,
          documentProcessorRunMetadata = metadata,
          documentProcessorRunInitialOutput = initialOutput,
          documentProcessorRunReviewedOutput = reviewedOutput
        }

instance ToJSON DocumentProcessorRun where
  toJSON DocumentProcessorRun {..} =
    Aeson.object $
      catMaybes
        [ Just ("object" .= documentProcessorRunObject),
          Just ("id" .= documentProcessorRunId),
          Just ("processorId" .= documentProcessorRunProcessorId),
          Just ("processorVersionId" .= documentProcessorRunProcessorVersionId),
          Just ("processorName" .= documentProcessorRunProcessorName),
          Just ("status" .= documentProcessorRunStatus),
          Just ("output" .= documentProcessorRunOutput),
          Just ("reviewed" .= documentProcessorRunReviewed),
          Just ("edited" .= documentProcessorRunEdited),
          Just ("edits" .= documentProcessorRunEdits),
          ("type" .=) <$> documentProcessorRunType,
          ("config" .=) <$> documentProcessorRunConfig,
          ("files" .=) <$> documentProcessorRunFiles,
          ("mergedProcessors" .=) <$> documentProcessorRunMergedProcessors,
          ("url" .=) <$> documentProcessorRunUrl,
          ("failureReason" .=) <$> documentProcessorRunFailureReason,
          ("failureMessage" .=) <$> documentProcessorRunFailureMessage,
          ("metadata" .=) <$> documentProcessorRunMetadata,
          ("initialOutput" .=) <$> documentProcessorRunInitialOutput,
          ("reviewedOutput" .=) <$> documentProcessorRunReviewedOutput
        ]

-- | A workflow run
data WorkflowRun = WorkflowRun
  { -- | Type of the object
    workflowRunObject :: ObjectType,
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
    workflowRunWorkflowId :: Maybe Text,
    -- | The name of the workflow that was run
    workflowRunWorkflowName :: Maybe Text,
    -- | The ID of the workflow version that was run
    workflowRunWorkflowVersionId :: Maybe Text,
    -- | When the workflow run was created
    workflowRunCreatedAt :: Maybe UTCTime,
    -- | When the workflow run was last updated
    workflowRunUpdatedAt :: Maybe UTCTime,
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
    workflowRunOutputs :: Maybe [DocumentProcessorRun],
    -- | Workflow step runs
    workflowRunStepRuns :: Maybe [WorkflowStepRun],
    -- | The associated workflow
    workflowRunWorkflow :: Maybe WorkflowSummary,
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
    objectType <- v Aeson..: "object"
    runId <- v Aeson..: "id"
    name <- v Aeson..:? "name"
    url <- v Aeson..:? "url"
    status <- v Aeson..: "status"
    metadata <- v Aeson..:? "metadata"
    files <- v Aeson..:? "files"
    workflowId <- v Aeson..:? "workflowId"
    workflowName <- v Aeson..:? "workflowName"
    workflowVersionId <- v Aeson..:? "workflowVersionId"
    createdAt <- v Aeson..:? "createdAt"
    updatedAt <- v Aeson..:? "updatedAt"
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
          workflowRunId = runId,
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
        [ Just ("object" .= workflowRunObject),
          Just ("id" .= workflowRunId),
          ("name" .=) <$> workflowRunName,
          ("url" .=) <$> workflowRunUrl,
          Just ("status" .= workflowRunStatus),
          ("metadata" .=) <$> workflowRunMetadata,
          ("files" .=) <$> workflowRunFiles,
          ("workflowId" .=) <$> workflowRunWorkflowId,
          ("workflowName" .=) <$> workflowRunWorkflowName,
          ("workflowVersionId" .=) <$> workflowRunWorkflowVersionId,
          ("createdAt" .=) <$> workflowRunCreatedAt,
          ("updatedAt" .=) <$> workflowRunUpdatedAt,
          ("initialRunAt" .=) <$> workflowRunInitialRunAt,
          Just ("reviewed" .= workflowRunReviewed),
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
    createdWorkflowId <- v Aeson..: "id"
    version <- v Aeson..: "version"
    name <- v Aeson..: "name"
    pure
      CreatedWorkflow
        { createdWorkflowObject = objectType,
          createdWorkflowId = createdWorkflowId,
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
    createdWorkflowRunOutputs :: Maybe [DocumentProcessorRun],
    -- | Workflow step runs
    createdWorkflowRunStepRuns :: Maybe [WorkflowStepRun],
    -- | The associated workflow
    createdWorkflowRunWorkflow :: Maybe WorkflowSummary,
    -- | The batch ID if part of a batch
    createdWorkflowRunBatchId :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON CreatedWorkflowRun where
  parseJSON = Aeson.withObject "CreatedWorkflowRun" $ \v -> do
    objectType <- v Aeson..: "object"
    runId <- v Aeson..: "id"
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
          createdWorkflowRunId = runId,
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

-- | A workflow run summary for list responses
data WorkflowRunSummary = WorkflowRunSummary
  { -- | ID of the workflow run
    workflowRunSummaryId :: Text,
    -- | Status of the workflow run
    workflowRunSummaryStatus :: WorkflowRunStatus,
    -- | When the workflow run was initially created
    workflowRunSummaryInitialRunAt :: Maybe UTCTime,
    -- | Whether the workflow run has been reviewed
    workflowRunSummaryReviewed :: Bool,
    -- | When the run started
    workflowRunSummaryStartTime :: Maybe UTCTime,
    -- | The ID of the workflow that was run
    workflowRunSummaryWorkflowId :: Text,
    -- | The name of the workflow that was run
    workflowRunSummaryWorkflowName :: Text,
    -- | The ID of the workflow version that was run
    workflowRunSummaryWorkflowVersionId :: Text,
    -- | The batch ID if part of a batch
    workflowRunSummaryBatchId :: Maybe Text,
    -- | When the workflow run was created
    workflowRunSummaryCreatedAt :: UTCTime,
    -- | When the workflow run was last updated
    workflowRunSummaryUpdatedAt :: UTCTime,
    -- | Name of the workflow run
    workflowRunSummaryName :: Maybe Text,
    -- | Dashboard URL for the workflow run
    workflowRunSummaryUrl :: Maybe Text,
    -- | When the run ended
    workflowRunSummaryEndTime :: Maybe UTCTime,
    -- | Files associated with the workflow run
    workflowRunSummaryFiles :: Maybe [File]
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON WorkflowRunSummary where
  parseJSON = Aeson.withObject "WorkflowRunSummary" $ \v -> do
    workflowRunSummaryId <- v Aeson..: "id"
    status <- v Aeson..: "status"
    initialRunAt <- v Aeson..:? "initialRunAt"
    reviewed <- v Aeson..: "reviewed"
    startTime <- v Aeson..:? "startTime"
    workflowId <- v Aeson..: "workflowId"
    workflowName <- v Aeson..: "workflowName"
    workflowVersionId <- v Aeson..: "workflowVersionId"
    batchId <- v Aeson..:? "batchId"
    createdAt <- v Aeson..: "createdAt"
    updatedAt <- v Aeson..: "updatedAt"
    name <- v Aeson..:? "name"
    url <- v Aeson..:? "url"
    endTime <- v Aeson..:? "endTime"
    files <- v Aeson..:? "files"
    pure
      WorkflowRunSummary
        { workflowRunSummaryId = workflowRunSummaryId,
          workflowRunSummaryStatus = status,
          workflowRunSummaryInitialRunAt = initialRunAt,
          workflowRunSummaryReviewed = reviewed,
          workflowRunSummaryStartTime = startTime,
          workflowRunSummaryWorkflowId = workflowId,
          workflowRunSummaryWorkflowName = workflowName,
          workflowRunSummaryWorkflowVersionId = workflowVersionId,
          workflowRunSummaryBatchId = batchId,
          workflowRunSummaryCreatedAt = createdAt,
          workflowRunSummaryUpdatedAt = updatedAt,
          workflowRunSummaryName = name,
          workflowRunSummaryUrl = url,
          workflowRunSummaryEndTime = endTime,
          workflowRunSummaryFiles = files
        }

instance ToJSON WorkflowRunSummary where
  toJSON WorkflowRunSummary {..} =
    Aeson.object $
      catMaybes
        [ Just ("id" .= workflowRunSummaryId),
          Just ("status" .= workflowRunSummaryStatus),
          ("initialRunAt" .=) <$> workflowRunSummaryInitialRunAt,
          Just ("reviewed" .= workflowRunSummaryReviewed),
          ("startTime" .=) <$> workflowRunSummaryStartTime,
          Just ("workflowId" .= workflowRunSummaryWorkflowId),
          Just ("workflowName" .= workflowRunSummaryWorkflowName),
          Just ("workflowVersionId" .= workflowRunSummaryWorkflowVersionId),
          ("batchId" .=) <$> workflowRunSummaryBatchId,
          Just ("createdAt" .= workflowRunSummaryCreatedAt),
          Just ("updatedAt" .= workflowRunSummaryUpdatedAt),
          ("name" .=) <$> workflowRunSummaryName,
          ("url" .=) <$> workflowRunSummaryUrl,
          ("endTime" .=) <$> workflowRunSummaryEndTime,
          ("files" .=) <$> workflowRunSummaryFiles
        ]

-- | Response from listing workflow runs
data ListWorkflowRunsResponse = ListWorkflowRunsResponse
  { -- | Whether the request was successful
    listWorkflowRunsResponseSuccess :: Bool,
    -- | The workflow runs
    listWorkflowRunsResponseWorkflowRuns :: [WorkflowRunSummary],
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