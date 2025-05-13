-- | Workflow operations for the Extend API
module Extend.V1.Workflows
    ( -- * Types
      Workflow(..)
    , WorkflowStep(..)
    , WorkflowStepRun(..)
    , ValidationRule(..)
    , StepRunOutput(..)
    , WorkflowRun(..)
    , WorkflowRunStatus(..)
    , ExtendFile(..)
    , PredeterminedOutput(..)
    , GetWorkflowResponse(..)
    , ListWorkflowsResponse(..)
    , RunWorkflowRequest(..)
    , RunWorkflowResponse(..)
    , GetWorkflowRunResponse(..)
    , ListWorkflowRunsResponse(..)
      -- * API
    , WorkflowsAPI
    , getWorkflow
    , listWorkflows
    , runWorkflow
    , getWorkflowRun
    , listWorkflowRuns
    ) where

import Extend.Prelude
import Extend.V1.Common
import Extend.V1.Files (File)
import Extend.V1.Processors (ProcessorRun)

-- | A workflow step
data WorkflowStep = WorkflowStep
    { object :: ObjectType
    -- ^ Type of the object
    , id :: Text
    -- ^ ID of the step
    , name :: Text
    -- ^ Name of the step
    , type_ :: Text
    -- ^ Type of the step
    } deriving stock (Show, Eq, Generic)

instance FromJSON WorkflowStep where
    parseJSON = withObject "WorkflowStep" $ \v -> do
        objectType <- v .: "object"
        id <- v .: "id"
        name <- v .: "name"
        type_ <- v .: "type"
        pure WorkflowStep
            { object = objectType
            , id = id
            , name = name
            , type_ = type_
            }

instance ToJSON WorkflowStep where
    toJSON WorkflowStep{..} = object
        [ "object" .= object
        , "id" .= id
        , "name" .= name
        , "type" .= type_
        ]

-- | Validation rule for a workflow step
data ValidationRule = ValidationRule
    { name :: Text
    -- ^ Name of the rule
    , valid :: Bool
    -- ^ Whether the rule is valid
    , validArray :: Maybe [Text]
    -- ^ Array of valid values
    , failureReason :: Maybe Text
    -- ^ Reason for failure
    , error :: Maybe Text
    -- ^ Error message
    } deriving stock (Show, Eq, Generic)

instance FromJSON ValidationRule where
    parseJSON = withObject "ValidationRule" $ \v -> do
        name <- v .: "name"
        valid <- v .: "valid"
        validArray <- v .:? "validArray"
        failureReason <- v .:? "failureReason"
        error <- v .:? "error"
        pure ValidationRule{..}

instance ToJSON ValidationRule where
    toJSON ValidationRule{..} = object $ catMaybes
        [ Just ("name" .= name)
        , Just ("valid" .= valid)
        , ("validArray" .=) <$> validArray
        , ("failureReason" .=) <$> failureReason
        , ("error" .=) <$> error
        ]

-- | Output of a workflow step run
data StepRunOutput = StepRunOutput
    { rules :: Maybe [ValidationRule]
    -- ^ Validation rules
    } deriving stock (Show, Eq, Generic)

instance FromJSON StepRunOutput where
    parseJSON = withObject "StepRunOutput" $ \v -> do
        rules <- v .:? "rules"
        pure StepRunOutput{..}

instance ToJSON StepRunOutput where
    toJSON StepRunOutput{..} = object $ catMaybes
        [ ("rules" .=) <$> rules
        ]

-- | A workflow step run
data WorkflowStepRun = WorkflowStepRun
    { object :: ObjectType
    -- ^ Type of the object
    , id :: Text
    -- ^ ID of the step run
    , status :: Text
    -- ^ Status of the step run
    , step :: WorkflowStep
    -- ^ The step that was run
    , output :: Maybe StepRunOutput
    -- ^ Output from the step
    } deriving stock (Show, Eq, Generic)

instance FromJSON WorkflowStepRun where
    parseJSON = withObject "WorkflowStepRun" $ \v -> do
        objectType <- v .: "object"
        id <- v .: "id"
        status <- v .: "status"
        step <- v .: "step"
        output <- v .:? "output"
        pure WorkflowStepRun
            { object = objectType
            , id = id
            , status = status
            , step = step
            , output = output
            }

instance ToJSON WorkflowStepRun where
    toJSON WorkflowStepRun{..} = object $ catMaybes
        [ Just ("object" .= object)
        , Just ("id" .= id)
        , Just ("status" .= status)
        , Just ("step" .= step)
        , ("output" .=) <$> output
        ]

-- | A workflow
data Workflow = Workflow
    { object :: ObjectType
    -- ^ Type of the object
    , id :: Text
    -- ^ ID of the workflow
    , name :: Text
    -- ^ Name of the workflow
    , description :: Maybe Text
    -- ^ Description of the workflow
    , version :: Text
    -- ^ Version of the workflow
    , createdAt :: UTCTime
    -- ^ When the workflow was created
    , updatedAt :: UTCTime
    -- ^ When the workflow was last updated
    } deriving stock (Show, Eq, Generic)

instance FromJSON Workflow where
    parseJSON = withObject "Workflow" $ \v -> do
        objectType <- v .: "object"
        id <- v .: "id"
        name <- v .: "name"
        description <- v .:? "description"
        version <- v .: "version"
        createdAt <- v .: "createdAt"
        updatedAt <- v .: "updatedAt"
        pure Workflow
            { object = objectType
            , id = id
            , name = name
            , description = description
            , version = version
            , createdAt = createdAt
            , updatedAt = updatedAt
            }

instance ToJSON Workflow where
    toJSON Workflow{..} = object $ catMaybes
        [ Just ("object" .= object)
        , Just ("id" .= id)
        , Just ("name" .= name)
        , ("description" .=) <$> description
        , Just ("version" .= version)
        , Just ("createdAt" .= createdAt)
        , Just ("updatedAt" .= updatedAt)
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
    parseJSON = withText "WorkflowRunStatus" $ \case
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
    { object :: ObjectType
    -- ^ Type of the object
    , id :: Text
    -- ^ ID of the workflow run
    , name :: Text
    -- ^ Name of the workflow run
    , url :: Text
    -- ^ URL to view the workflow run
    , status :: WorkflowRunStatus
    -- ^ Status of the workflow run
    , metadata :: Map Text Value
    -- ^ Metadata associated with the run
    , files :: [File]
    -- ^ Files associated with the run
    , initialRunAt :: UTCTime
    -- ^ When the workflow run was initially created
    , reviewed :: Bool
    -- ^ Whether the workflow run has been reviewed
    , outputs :: [ProcessorRun]
    -- ^ Outputs from document processors
    , stepRuns :: [WorkflowStepRun]
    -- ^ Step runs in the workflow
    , workflow :: Workflow
    -- ^ The workflow this run belongs to
    , batchId :: Maybe Text
    -- ^ The batch ID if part of a batch
    , failureReason :: Maybe Text
    -- ^ Reason for failure if failed
    , failureMessage :: Maybe Text
    -- ^ Detailed failure message if failed
    , reviewedBy :: Maybe Text
    -- ^ Who reviewed the run
    , rejectionNote :: Maybe Text
    -- ^ Note if rejected
    , reviewedAt :: Maybe UTCTime
    -- ^ When the run was reviewed
    , startTime :: Maybe UTCTime
    -- ^ When the run started
    , endTime :: Maybe UTCTime
    -- ^ When the run ended
    } deriving stock (Show, Eq, Generic)

instance FromJSON WorkflowRun where
    parseJSON = withObject "WorkflowRun" $ \v -> do
        objectType <- v .: "object"
        id <- v .: "id"
        name <- v .: "name"
        url <- v .: "url"
        status <- v .: "status"
        metadata <- v .: "metadata"
        files <- v .: "files"
        initialRunAt <- v .: "initialRunAt"
        reviewed <- v .: "reviewed"
        outputs <- v .: "outputs"
        stepRuns <- v .: "stepRuns"
        workflow <- v .: "workflow"
        batchId <- v .:? "batchId"
        failureReason <- v .:? "failureReason"
        failureMessage <- v .:? "failureMessage"
        reviewedBy <- v .:? "reviewedBy"
        rejectionNote <- v .:? "rejectionNote"
        reviewedAt <- v .:? "reviewedAt"
        startTime <- v .:? "startTime"
        endTime <- v .:? "endTime"
        pure WorkflowRun{..}

instance ToJSON WorkflowRun where
    toJSON WorkflowRun{..} = object $ catMaybes
        [ Just ("object" .= object)
        , Just ("id" .= id)
        , Just ("name" .= name)
        , Just ("url" .= url)
        , Just ("status" .= status)
        , Just ("metadata" .= metadata)
        , Just ("files" .= files)
        , Just ("initialRunAt" .= initialRunAt)
        , Just ("reviewed" .= reviewed)
        , Just ("outputs" .= outputs)
        , Just ("stepRuns" .= stepRuns)
        , Just ("workflow" .= workflow)
        , ("batchId" .=) <$> batchId
        , ("failureReason" .=) <$> failureReason
        , ("failureMessage" .=) <$> failureMessage
        , ("reviewedBy" .=) <$> reviewedBy
        , ("rejectionNote" .=) <$> rejectionNote
        , ("reviewedAt" .=) <$> reviewedAt
        , ("startTime" .=) <$> startTime
        , ("endTime" .=) <$> endTime
        ]

-- | Predetermined output for a processor
data PredeterminedOutput = PredeterminedOutput
    { processorId :: Text
    -- ^ The ID of the processor that the output is associated with
    , output :: Value
    -- ^ The output that is being overridden
    } deriving stock (Show, Eq, Generic)
      deriving anyclass (FromJSON, ToJSON)

-- | File to process through a workflow
data ExtendFile = ExtendFile
    { fileName :: Maybe Text
    -- ^ The name of the file to be processed
    , fileUrl :: Maybe Text
    -- ^ A URL where the file can be downloaded from
    , fileId :: Maybe Text
    -- ^ Extend's internal ID for the file
    , outputs :: Maybe [PredeterminedOutput]
    -- ^ Optional predetermined outputs to override generated outputs
    } deriving stock (Show, Eq, Generic)

instance FromJSON ExtendFile where
    parseJSON = withObject "ExtendFile" $ \v -> do
        fileName <- v .:? "fileName"
        fileUrl <- v .:? "fileUrl"
        fileId <- v .:? "fileId"
        outputs <- v .:? "outputs"
        pure ExtendFile{..}

instance ToJSON ExtendFile where
    toJSON ExtendFile{..} = object $ catMaybes
        [ ("fileName" .=) <$> fileName
        , ("fileUrl" .=) <$> fileUrl
        , ("fileId" .=) <$> fileId
        , ("outputs" .=) <$> outputs
        ]

-- | Request to run a workflow
data RunWorkflowRequest = RunWorkflowRequest
    { workflowId :: Text
    -- ^ The ID of the workflow to run
    , files :: Maybe [ExtendFile]
    -- ^ Optional array of files to process through the workflow
    , rawTexts :: Maybe [Text]
    -- ^ Optional array of raw strings to be converted to text files
    , priority :: Maybe Int
    -- ^ Optional value (1-100) used to determine relative order of WorkflowRuns when rate limiting is in effect.
    -- Lower values will be prioritized before higher values. Defaults to 50.
    , metadata :: Maybe (Map Text Value)
    -- ^ Optional metadata object that can be assigned to a specific WorkflowRun to help identify it.
    -- It will be returned in the response and webhooks. You can place any arbitrary key : value pairs in this object.
    } deriving stock (Show, Eq, Generic)

instance ToJSON RunWorkflowRequest where
    toJSON RunWorkflowRequest{..} = object $ catMaybes
        [ Just ("workflowId" .= workflowId)
        , ("files" .=) <$> files
        , ("rawTexts" .=) <$> rawTexts
        , ("priority" .=) <$> priority
        , ("metadata" .=) <$> metadata
        ]

-- | Response from getting a workflow
newtype GetWorkflowResponse = GetWorkflowResponse
    { workflow :: Workflow
    -- ^ The requested workflow
    } deriving stock (Show, Eq, Generic)

instance FromJSON GetWorkflowResponse where
    parseJSON = withObject "GetWorkflowResponse" $ \v -> do
        workflow <- v .: "workflow"
        pure GetWorkflowResponse{..}

-- | Response from listing workflows
data ListWorkflowsResponse = ListWorkflowsResponse
    { workflows :: [Workflow]
    -- ^ The workflows
    , pagination :: Maybe Pagination
    -- ^ Pagination information
    } deriving stock (Show, Eq, Generic)

instance FromJSON ListWorkflowsResponse where
    parseJSON = withObject "ListWorkflowsResponse" $ \v -> do
        workflows <- v .: "workflows"
        pagination <- v .:? "pagination"
        pure ListWorkflowsResponse{..}

-- | Response from running a workflow
data RunWorkflowResponse = RunWorkflowResponse
    { workflowRuns :: [WorkflowRun]
    -- ^ The workflow runs that were created
    } deriving stock (Show, Eq, Generic)

instance FromJSON RunWorkflowResponse where
    parseJSON = withObject "RunWorkflowResponse" $ \v -> do
        workflowRuns <- v .: "workflowRuns"
        pure RunWorkflowResponse{..}

-- | Response from getting a workflow run
newtype GetWorkflowRunResponse = GetWorkflowRunResponse
    { workflowRun :: WorkflowRun
    -- ^ The requested workflow run
    } deriving stock (Show, Eq, Generic)

instance FromJSON GetWorkflowRunResponse where
    parseJSON = withObject "GetWorkflowRunResponse" $ \v -> do
        workflowRun <- v .: "workflowRun"
        pure GetWorkflowRunResponse{..}

-- | Response from listing workflow runs
data ListWorkflowRunsResponse = ListWorkflowRunsResponse
    { workflowRuns :: [WorkflowRun]
    -- ^ The workflow runs
    , pagination :: Maybe Pagination
    -- ^ Pagination information
    } deriving stock (Show, Eq, Generic)

instance FromJSON ListWorkflowRunsResponse where
    parseJSON = withObject "ListWorkflowRunsResponse" $ \v -> do
        workflowRuns <- v .: "workflowRuns"
        pagination <- v .:? "pagination"
        pure ListWorkflowRunsResponse{..}

-- | Workflows API endpoints
type WorkflowsAPI =
    "workflows" :> Capture "workflowId" Text
        :> Header' '[Required, Strict] "Authorization" Text
        :> Header' '[Required, Strict] "x-extend-api-version" Text
        :> Get '[JSON] (SuccessResponse GetWorkflowResponse)
    :<|>
    "workflows"
        :> Header' '[Required, Strict] "Authorization" Text
        :> Header' '[Required, Strict] "x-extend-api-version" Text
        :> QueryParam "limit" Int
        :> QueryParam "page" Int
        :> Get '[JSON] (SuccessResponse ListWorkflowsResponse)
    :<|>
    "workflow_runs"
        :> Header' '[Required, Strict] "Authorization" Text
        :> Header' '[Required, Strict] "x-extend-api-version" Text
        :> ReqBody '[JSON] RunWorkflowRequest
        :> Post '[JSON] (SuccessResponse RunWorkflowResponse)
    :<|>
    "workflow_runs" :> Capture "runId" Text
        :> Header' '[Required, Strict] "Authorization" Text
        :> Header' '[Required, Strict] "x-extend-api-version" Text
        :> Get '[JSON] (SuccessResponse GetWorkflowRunResponse)
    :<|>
    "workflow_runs"
        :> Header' '[Required, Strict] "Authorization" Text
        :> Header' '[Required, Strict] "x-extend-api-version" Text
        :> QueryParam "workflowId" Text
        :> QueryParam "limit" Int
        :> QueryParam "page" Int
        :> Get '[JSON] (SuccessResponse ListWorkflowRunsResponse)

-- | Get a workflow
getWorkflow
    :: ApiToken
    -> ApiVersion
    -> Text -- ^ Workflow ID
    -> ClientM (SuccessResponse GetWorkflowResponse)
getWorkflow (ApiToken token) (ApiVersion version) workflowId =
    client (Proxy @WorkflowsAPI) workflowId ("Bearer " <> token) version

-- | List workflows
listWorkflows
    :: ApiToken
    -> ApiVersion
    -> Maybe Int -- ^ Limit
    -> Maybe Int -- ^ Page
    -> ClientM (SuccessResponse ListWorkflowsResponse)
listWorkflows (ApiToken token) (ApiVersion version) =
    client (Proxy @WorkflowsAPI) ("Bearer " <> token) version

-- | Run a workflow
runWorkflow
    :: ApiToken
    -> ApiVersion
    -> RunWorkflowRequest
    -> ClientM (SuccessResponse RunWorkflowResponse)
runWorkflow (ApiToken token) (ApiVersion version) =
    client (Proxy @WorkflowsAPI) ("Bearer " <> token) version

-- | Get a workflow run
getWorkflowRun
    :: ApiToken
    -> ApiVersion
    -> Text -- ^ Run ID
    -> ClientM (SuccessResponse GetWorkflowRunResponse)
getWorkflowRun (ApiToken token) (ApiVersion version) runId =
    client (Proxy @WorkflowsAPI) runId ("Bearer " <> token) version

-- | List workflow runs
listWorkflowRuns
    :: ApiToken
    -> ApiVersion
    -> Maybe Text -- ^ Workflow ID
    -> Maybe Int -- ^ Limit
    -> Maybe Int -- ^ Page
    -> ClientM (SuccessResponse ListWorkflowRunsResponse)
listWorkflowRuns (ApiToken token) (ApiVersion version) =
    client (Proxy @WorkflowsAPI) ("Bearer " <> token) version 