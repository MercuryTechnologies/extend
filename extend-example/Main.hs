{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Example usage of the Extend API with command-line argument parsing
module Main (main) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (isJust)
import Data.Text (Text, pack, unpack)
import Extend.V1
import qualified Extend.V1.Processors as Processors
import qualified Extend.V1.Workflows as Workflows
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTPS
import Options.Applicative
import qualified Servant.Client as Client
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.IO (hPrint, hPutStrLn, stderr)
import Prelude

-- | CLI command options
data Command
  = RunWorkflow Text [Text] Bool (Maybe Text) (Maybe Text) -- workflowId, fileUrls, isLocalFile flag, optional file name, optional version
  | BatchRunWorkflow Text [Text] Bool (Maybe Text) (Maybe Text) -- workflowId, fileUrls, isLocalFile flag, optional file name, optional version
  | ListWorkflowRuns (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Int)
  | GetWorkflowRun Text
  | CreateWorkflow Text -- workflow name
  | RunProcessor Text Text Bool (Maybe Text) -- processorId, fileUrl/path, isLocalFile flag, optional version
  | GetProcessorRun Text -- processor run ID
  | CreateProcessor Text Text -- processor name, processor type (EXTRACT, CLASSIFY, SPLITTER)
  | UpdateProcessor Text Text -- processor ID, new name
  | GetProcessorVersion Text Text -- processor ID, processor version ID
  | ListProcessorVersions Text -- processor ID
  | PublishProcessorVersion Text Text (Maybe Text) -- processor ID, release type (major/minor), optional description
  | GetBatchProcessorRun Text -- batch processor run ID
  deriving (Show)

-- | Parse CLI arguments
commandParser :: Parser Command
commandParser =
  subparser $
    mconcat
      [ command
          "run-workflow"
          ( info
              ( RunWorkflow
                  <$> workflowIdArg
                  <*> many (strArgument (metavar "FILE" <> help "File URL or path to process"))
                  <*> switch (long "local" <> help "Treat files as local file paths instead of URLs")
                  <*> optional (strOption (long "filename" <> metavar "NAME" <> help "Name to use for the file(s)"))
                  <*> optional (strOption (long "version" <> metavar "VERSION" <> help "Workflow version to run (e.g., '3' or 'draft')"))
              )
              (progDesc "Run a workflow with specified files (default: URLs)")
          ),
        command
          "batch-run-workflow"
          ( info
              ( BatchRunWorkflow
                  <$> workflowIdArg
                  <*> many (strArgument (metavar "FILE" <> help "File URL or path to process"))
                  <*> switch (long "local" <> help "Treat files as local file paths instead of URLs")
                  <*> optional (strOption (long "filename" <> metavar "NAME" <> help "Name to use for the file(s)"))
                  <*> optional (strOption (long "version" <> metavar "VERSION" <> help "Workflow version to run (e.g., '3' or 'draft')"))
              )
              (progDesc "Run a batch of workflows with specified files (default: URLs)")
          ),
        command
          "list-runs"
          ( info
              ( ListWorkflowRuns
                  <$> optional workflowIdOpt
                  <*> optional statusOpt
                  <*> optional fileNameContainsOpt
                  <*> optional sortByOpt
                  <*> optional sortDirOpt
                  <*> optional nextPageTokenOpt
                  <*> optional limitOpt
              )
              (progDesc "List workflow runs with various filtering options")
          ),
        command
          "get-run"
          ( info
              (GetWorkflowRun <$> workflowRunIdArg)
              (progDesc "Get a specific workflow run by ID")
          ),
        command
          "create-workflow"
          ( info
              (CreateWorkflow <$> workflowNameArg)
              (progDesc "Create a new workflow with the specified name")
          ),
        command
          "run-processor"
          ( info
              ( RunProcessor
                  <$> processorIdArg
                  <*> fileArg
                  <*> switch (long "local" <> help "Treat file as a local file path instead of URL")
                  <*> optional (strOption (long "version" <> metavar "VERSION" <> help "Processor version to run (e.g., 'latest' or 'draft')"))
              )
              (progDesc "Run a processor with a specified file")
          ),
        command
          "get-processor-run"
          ( info
              (GetProcessorRun <$> processorRunIdArg)
              (progDesc "Get details about a processor run")
          ),
        command
          "create-processor"
          ( info
              ( CreateProcessor
                  <$> processorNameArg
                  <*> processorTypeArg
              )
              (progDesc "Create a new processor")
          ),
        command
          "update-processor"
          ( info
              ( UpdateProcessor
                  <$> processorIdArg
                  <*> processorNameArg
              )
              (progDesc "Update an existing processor")
          ),
        command
          "get-processor-version"
          ( info
              ( GetProcessorVersion
                  <$> processorIdArg
                  <*> processorVersionIdArg
              )
              (progDesc "Get a specific processor version")
          ),
        command
          "list-processor-versions"
          ( info
              (ListProcessorVersions <$> processorIdArg)
              (progDesc "List all versions of a processor")
          ),
        command
          "publish-processor-version"
          ( info
              ( PublishProcessorVersion
                  <$> processorIdArg
                  <*> releaseTypeArg
                  <*> optional (strOption (long "description" <> metavar "DESCRIPTION" <> help "Description of the changes in this version"))
              )
              (progDesc "Publish a new version of a processor")
          ),
        command
          "get-batch-processor-run"
          ( info
              (GetBatchProcessorRun <$> batchProcessorRunIdArg)
              (progDesc "Get details about a batch processor run")
          )
      ]

-- | Common CLI arguments and options
workflowIdArg :: Parser Text
workflowIdArg =
  strArgument
    (metavar "WORKFLOW_ID" <> help "Workflow ID")

workflowIdOpt :: Parser Text
workflowIdOpt =
  strOption
    (long "workflow-id" <> short 'w' <> metavar "WORKFLOW_ID" <> help "Filter by workflow ID")

statusOpt :: Parser Text
statusOpt =
  strOption
    (long "status" <> short 's' <> metavar "STATUS" <> help "Filter by status (PENDING, PROCESSING, NEEDS_REVIEW, REJECTED, PROCESSED, FAILED)")

fileNameContainsOpt :: Parser Text
fileNameContainsOpt =
  strOption
    (long "filename" <> short 'f' <> metavar "FILENAME" <> help "Filter by file name containing this string")

sortByOpt :: Parser Text
sortByOpt =
  strOption
    (long "sort-by" <> metavar "FIELD" <> help "Sort by field (updatedAt, createdAt)")

sortDirOpt :: Parser Text
sortDirOpt =
  strOption
    (long "sort-dir" <> metavar "DIRECTION" <> help "Sort direction (asc, desc)")

nextPageTokenOpt :: Parser Text
nextPageTokenOpt =
  strOption
    (long "next-page-token" <> short 'n' <> metavar "TOKEN" <> help "Token for getting the next page of results")

workflowRunIdArg :: Parser Text
workflowRunIdArg =
  strArgument
    (metavar "RUN_ID" <> help "Workflow run ID")

limitOpt :: Parser Int
limitOpt =
  option
    auto
    (long "limit" <> short 'l' <> metavar "LIMIT" <> help "Limit the number of results (sets maxPageSize parameter)")

workflowNameArg :: Parser Text
workflowNameArg =
  strArgument
    (metavar "NAME" <> help "Name for the new workflow")

-- | Additional arguments for processor commands
processorIdArg :: Parser Text
processorIdArg =
  strArgument
    (metavar "PROCESSOR_ID" <> help "Processor ID")

fileArg :: Parser Text
fileArg =
  strArgument
    (metavar "FILE" <> help "File URL or path to process")

processorRunIdArg :: Parser Text
processorRunIdArg =
  strArgument
    (metavar "PROCESSOR_RUN_ID" <> help "Processor run ID")

processorNameArg :: Parser Text
processorNameArg =
  strArgument
    (metavar "NAME" <> help "Name for the processor")

processorTypeArg :: Parser Text
processorTypeArg =
  strArgument
    (metavar "TYPE" <> help "Type of processor (EXTRACT, CLASSIFY, SPLITTER)")

processorVersionIdArg :: Parser Text
processorVersionIdArg =
  strArgument
    (metavar "VERSION_ID" <> help "Processor version ID")

releaseTypeArg :: Parser Text
releaseTypeArg =
  strArgument
    (metavar "RELEASE_TYPE" <> help "Release type (major or minor)")

batchProcessorRunIdArg :: Parser Text
batchProcessorRunIdArg =
  strArgument
    (metavar "BATCH_PROCESSOR_RUN_ID" <> help "Batch processor run ID")

-- | Main entry point
main :: IO ()
main = do
  cliCommand <- execParser opts

  -- Get API key from environment variable
  maybeApiKey <- lookupEnv "EXTEND_API_KEY"
  apiKey <- case maybeApiKey of
    Just key -> pure (pack key)
    Nothing -> do
      hPutStrLn stderr "EXTEND_API_KEY environment variable not set"
      exitFailure

  -- Base URL for the API
  let baseUrl = "api.extend.ai"

  -- Create a client environment
  env <- createClientEnv baseUrl

  -- Create API token and use default version
  let token = ApiToken apiKey

  result <- runCommand token defaultApiVersion env cliCommand
  case result of
    Left err -> do
      hPutStrLn stderr "API Error:"
      hPrint stderr err
      exitFailure
    Right () -> pure ()

-- | Run the appropriate command
runCommand :: ApiToken -> ApiVersion -> Client.ClientEnv -> Command -> IO (Either Client.ClientError ())
runCommand token version env = \case
  RunWorkflow workflowId filePaths isLocal maybeFileName maybeVersion -> do
    putStrLn $ "Running workflow: " ++ unpack workflowId
    when (isJust maybeVersion) $
      putStrLn $
        "Version: " ++ maybe "latest" unpack maybeVersion
    putStrLn $ "Files: " ++ show filePaths
    when (isJust maybeFileName) $
      putStrLn $
        "Using filename: " ++ maybe "" unpack maybeFileName
    when (null filePaths) $
      putStrLn "Warning: No files specified. This will create an empty workflow run."

    -- Create file objects for the request
    let files =
          map
            ( \f ->
                if isLocal
                  then Prelude.error "Local file processing not implemented yet"
                  else
                    Workflows.ExtendFile
                      { Workflows.extendFileName = maybeFileName,
                        Workflows.extendFileUrl = Just f,
                        Workflows.extendFileId = Nothing,
                        Workflows.extendFileOutputs = Nothing
                      }
            )
            filePaths

    let request =
          Workflows.RunWorkflowRequest
            { Workflows.runWorkflowRequestWorkflowId = workflowId,
              Workflows.runWorkflowRequestFiles = if null files then Nothing else Just files,
              Workflows.runWorkflowRequestRawTexts = Nothing,
              Workflows.runWorkflowRequestPriority = Nothing,
              Workflows.runWorkflowRequestMetadata = Nothing,
              Workflows.runWorkflowRequestVersion = maybeVersion
            }
    Client.runClientM (runWorkflowCmd token version request) env
  BatchRunWorkflow extendWorkflowId filePaths isLocal maybeFileName maybeVersion -> do
    putStrLn $ "Batch running workflow: " ++ unpack extendWorkflowId
    when (isJust maybeVersion) $
      putStrLn $
        "Version: " ++ maybe "latest" unpack maybeVersion
    putStrLn $ "Files: " ++ show filePaths
    when (isJust maybeFileName) $
      putStrLn $
        "Using filename: " ++ maybe "" unpack maybeFileName
    when (null filePaths) $
      putStrLn "Warning: No files specified. This will create an empty batch workflow run."

    -- Create input objects for the request
    let inputs =
          map
            ( \f ->
                let file =
                      if isLocal
                        then Prelude.error "Local file processing not implemented yet"
                        else
                          Workflows.BatchInputFile
                            { Workflows.batchInputFileName = maybeFileName,
                              Workflows.batchInputFileUrl = Just f,
                              Workflows.batchInputFileId = Nothing
                            }
                 in Workflows.BatchWorkflowInput
                      { Workflows.batchWorkflowInputFile = Just file,
                        Workflows.batchWorkflowInputRawText = Nothing,
                        Workflows.batchWorkflowInputMetadata = Nothing,
                        Workflows.batchWorkflowInputSecrets = Nothing
                      }
            )
            filePaths

    let request =
          Workflows.BatchRunWorkflowRequest
            { Workflows.batchRunWorkflowRequestWorkflowId = extendWorkflowId,
              Workflows.batchRunWorkflowRequestInputs = inputs,
              Workflows.batchRunWorkflowRequestVersion = maybeVersion
            }
    Client.runClientM (batchRunWorkflowCmd token version request) env
  ListWorkflowRuns maybeWorkflowId maybeStatus maybeFileNameContains maybeSortBy maybeSortDir maybeNextPageToken maybeLimit -> do
    putStrLn "Listing workflow runs..."

    -- Show filter information if present
    when (isJust maybeWorkflowId) $
      putStrLn $
        "Filtered by workflow ID: "
          ++ maybe "" unpack maybeWorkflowId
    when (isJust maybeStatus) $
      putStrLn $
        "Filtered by status: "
          ++ maybe "" unpack maybeStatus
    when (isJust maybeFileNameContains) $
      putStrLn $
        "Filtered by filename containing: "
          ++ maybe "" unpack maybeFileNameContains
    when (isJust maybeSortBy) $
      putStrLn $
        "Sorted by: "
          ++ maybe "" unpack maybeSortBy
    when (isJust maybeSortDir) $
      putStrLn $
        "Sort direction: "
          ++ maybe "" unpack maybeSortDir
    when (isJust maybeNextPageToken) $
      putStrLn $
        "Using page token: "
          ++ maybe "" unpack maybeNextPageToken
    when (isJust maybeLimit) $
      putStrLn $
        "Limited to: "
          ++ maybe "" show maybeLimit
          ++ " results"

    Client.runClientM (listWorkflowRunsCmd token version maybeWorkflowId maybeStatus maybeFileNameContains maybeSortBy maybeSortDir maybeNextPageToken maybeLimit) env
  GetWorkflowRun runId -> do
    putStrLn $ "Getting workflow run: " ++ unpack runId
    Client.runClientM (getWorkflowRunCmd token version runId) env
  CreateWorkflow workflowName -> do
    putStrLn $ "Creating workflow: " ++ unpack workflowName

    let request =
          Workflows.CreateWorkflowRequest
            { Workflows.createWorkflowRequestName = workflowName
            }

    Client.runClientM (createWorkflowCmd token version request) env
  RunProcessor processorId filePath isLocal maybeVersion -> do
    putStrLn $ "Running processor: " ++ unpack processorId
    when (isJust maybeVersion) $
      putStrLn $
        "Version: " ++ maybe "latest" unpack maybeVersion
    putStrLn $ "File: " ++ unpack filePath

    -- Create file input for the request
    let file =
          if isLocal
            then Prelude.error "Local file processing not implemented yet"
            else
              Processors.ProcessorRunFileInput
                { Processors.processorRunFileInputFileName = Nothing,
                  Processors.processorRunFileInputFileUrl = Just filePath,
                  Processors.processorRunFileInputFileId = Nothing
                }

    let request =
          Processors.RunProcessorRequest
            { Processors.runProcessorRequestProcessorId = processorId,
              Processors.runProcessorRequestVersion = maybeVersion,
              Processors.runProcessorRequestFile = Just file,
              Processors.runProcessorRequestRawText = Nothing,
              Processors.runProcessorRequestPriority = Nothing,
              Processors.runProcessorRequestMetadata = Nothing,
              Processors.runProcessorRequestConfig = Nothing
            }
    Client.runClientM (runProcessorCmd token version request) env
  GetProcessorRun runId -> do
    putStrLn $ "Getting processor run: " ++ unpack runId
    Client.runClientM (getProcessorRunCmd token version runId) env
  CreateProcessor name type_ -> do
    putStrLn $ "Creating processor: " ++ unpack name
    putStrLn $ "Type: " ++ unpack type_

    let processorType = case unpack type_ of
          "EXTRACT" -> Processors.Extract
          "CLASSIFY" -> Processors.Classify
          "SPLITTER" -> Processors.Splitter
          _ -> Prelude.error $ "Unknown processor type: " ++ unpack type_

    let request =
          Processors.CreateProcessorRequest
            { Processors.createProcessorRequestName = name,
              Processors.createProcessorRequestType = processorType,
              Processors.createProcessorRequestCloneProcessorId = Nothing,
              Processors.createProcessorRequestConfig = Nothing
            }
    Client.runClientM (createProcessorCmd token version request) env
  UpdateProcessor processorId name -> do
    putStrLn $ "Updating processor: " ++ unpack processorId
    putStrLn $ "New name: " ++ unpack name

    let request =
          Processors.UpdateProcessorRequest
            { Processors.updateProcessorRequestName = Just name,
              Processors.updateProcessorRequestConfig = Nothing
            }
    Client.runClientM (updateProcessorCmd token version processorId request) env
  GetProcessorVersion processorId versionId -> do
    putStrLn $ "Getting processor version: " ++ unpack processorId
    putStrLn $ "Version ID: " ++ unpack versionId
    Client.runClientM (getProcessorVersionCmd token version processorId versionId) env
  ListProcessorVersions processorId -> do
    putStrLn $ "Listing processor versions: " ++ unpack processorId
    Client.runClientM (listProcessorVersionsCmd token version processorId) env
  PublishProcessorVersion processorId releaseType maybeDescription -> do
    putStrLn $ "Publishing processor version: " ++ unpack processorId
    putStrLn $ "Release type: " ++ unpack releaseType
    when (isJust maybeDescription) $
      putStrLn $
        "Description: " ++ maybe "" unpack maybeDescription

    let request =
          Processors.PublishProcessorVersionRequest
            { Processors.publishProcessorVersionRequestReleaseType = releaseType,
              Processors.publishProcessorVersionRequestDescription = maybeDescription,
              Processors.publishProcessorVersionRequestConfig = Nothing
            }
    Client.runClientM (publishProcessorVersionCmd token version processorId request) env
  GetBatchProcessorRun batchRunId -> do
    putStrLn $ "Getting batch processor run: " ++ unpack batchRunId
    Client.runClientM (getBatchProcessorRunCmd token version batchRunId) env

-- | Command to run a workflow
runWorkflowCmd :: ApiToken -> ApiVersion -> Workflows.RunWorkflowRequest -> Client.ClientM ()
runWorkflowCmd token version request = do
  response <- runWorkflow token version request
  let Workflows.RunWorkflowResponse
        { Workflows.runWorkflowResponseSuccess = success,
          Workflows.runWorkflowResponseWorkflowRuns = workflowRuns
        } = response
  liftIO $ do
    putStrLn $ "Success: " ++ show success
    putStrLn $ "Created " ++ show (length workflowRuns) ++ " workflow runs:"
    mapM_
      ( \run -> do
          putStrLn $ "  - Run ID: " ++ unpack (Workflows.createdWorkflowRunId run)
          putStrLn $ "    Status: " ++ show (Workflows.createdWorkflowRunStatus run)
          case Workflows.createdWorkflowRunUrl run of
            Just url -> putStrLn $ "    URL: " ++ unpack url
            Nothing -> pure ()
          case Workflows.createdWorkflowRunWorkflow run of
            Just workflow -> do
              putStrLn $ "    Workflow: " ++ unpack (Workflows.workflowSummaryName workflow)
              putStrLn $ "    Version: " ++ unpack (Workflows.workflowSummaryVersion workflow)
            Nothing -> pure ()
      )
      workflowRuns

-- | Command to list workflow runs
listWorkflowRunsCmd :: ApiToken -> ApiVersion -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int -> Client.ClientM ()
listWorkflowRunsCmd token version maybeWorkflowId maybeStatus maybeFileNameContains maybeSortBy maybeSortDir maybeNextPageToken maybeLimit = do
  response <- listWorkflowRuns token version maybeWorkflowId maybeStatus maybeFileNameContains maybeSortBy maybeSortDir maybeNextPageToken maybeLimit
  let Workflows.ListWorkflowRunsResponse
        { Workflows.listWorkflowRunsResponseSuccess = success,
          Workflows.listWorkflowRunsResponseWorkflowRuns = workflowRuns,
          Workflows.listWorkflowRunsResponseNextPageToken = nextPageToken
        } = response
  liftIO $ do
    putStrLn $ "Success: " ++ show success
    putStrLn $ "Found " ++ show (length workflowRuns) ++ " workflow runs:"
    mapM_ printWorkflowRunSummary workflowRuns
    when (isJust nextPageToken) $
      putStrLn $
        "Next page token: "
          ++ maybe "" unpack nextPageToken

-- | Command to get a specific workflow run
getWorkflowRunCmd :: ApiToken -> ApiVersion -> Text -> Client.ClientM ()
getWorkflowRunCmd token extendApiVersion runId = do
  response <- getWorkflowRun token extendApiVersion runId
  let Workflows.GetWorkflowRunResponse
        { Workflows.getWorkflowRunResponseSuccess = isSuccess,
          Workflows.getWorkflowRunResponseWorkflowRun = workflowRun
        } = response
  liftIO $ do
    putStrLn $ "Success: " ++ show isSuccess
    printWorkflowRun workflowRun

-- | Helper to print a workflow run
printWorkflowRun :: Workflows.WorkflowRun -> IO ()
printWorkflowRun workflowRun = do
  putStrLn $ "  - Run: " ++ unpack (Workflows.workflowRunId workflowRun)
  putStrLn $ "    Status: " ++ show (Workflows.workflowRunStatus workflowRun)
  case Workflows.workflowRunWorkflowName workflowRun of
    Just name -> putStrLn $ "    Workflow Name: " ++ unpack name
    Nothing ->
      case Workflows.workflowRunWorkflow workflowRun of
        Just workflow -> putStrLn $ "    Workflow: " ++ unpack (Workflows.workflowSummaryName workflow)
        Nothing -> putStrLn "    Workflow: (unknown)"

  -- Display the batch ID if available
  case Workflows.workflowRunBatchId workflowRun of
    Just batchId -> putStrLn $ "    Batch ID: " ++ unpack batchId
    Nothing -> pure ()

  -- Display timestamps if available
  case Workflows.workflowRunInitialRunAt workflowRun of
    Just initialRunAt -> putStrLn $ "    Initial Run At: " ++ show initialRunAt
    Nothing -> putStrLn "    Initial Run At: (unknown)"

  case Workflows.workflowRunStartTime workflowRun of
    Just startTime -> putStrLn $ "    Start Time: " ++ show startTime
    Nothing -> putStrLn "    Start Time: (unknown)"

  case Workflows.workflowRunEndTime workflowRun of
    Just endTime -> putStrLn $ "    End Time: " ++ show endTime
    Nothing -> putStrLn "    End Time: (unknown)"

  putStrLn $ "    Reviewed: " ++ show (Workflows.workflowRunReviewed workflowRun)

  case Workflows.workflowRunReviewedAt workflowRun of
    Just reviewedAt -> putStrLn $ "    Reviewed At: " ++ show reviewedAt
    Nothing -> pure ()

  putStrLn ""

-- | Helper to print a workflow run summary
printWorkflowRunSummary :: Workflows.WorkflowRunSummary -> IO ()
printWorkflowRunSummary workflowRun = do
  putStrLn $ "  - Run: " ++ unpack (Workflows.workflowRunSummaryId workflowRun)
  putStrLn $ "    Status: " ++ show (Workflows.workflowRunSummaryStatus workflowRun)
  putStrLn $ "    Workflow: " ++ unpack (Workflows.workflowRunSummaryWorkflowName workflowRun)

  -- Display the batch ID if available
  case Workflows.workflowRunSummaryBatchId workflowRun of
    Just batchId -> putStrLn $ "    Batch ID: " ++ unpack batchId
    Nothing -> pure ()

  -- Display timestamps if available
  putStrLn $ "    Created at: " ++ show (Workflows.workflowRunSummaryCreatedAt workflowRun)

  case Workflows.workflowRunSummaryInitialRunAt workflowRun of
    Just initialRunAt -> putStrLn $ "    Initial Run At: " ++ show initialRunAt
    Nothing -> pure ()

  case Workflows.workflowRunSummaryStartTime workflowRun of
    Just startTime -> putStrLn $ "    Start Time: " ++ show startTime
    Nothing -> pure ()

  case Workflows.workflowRunSummaryEndTime workflowRun of
    Just endTime -> putStrLn $ "    End Time: " ++ show endTime
    Nothing -> pure ()

  putStrLn $ "    Reviewed: " ++ show (Workflows.workflowRunSummaryReviewed workflowRun)

  putStrLn ""

-- | Command to run a batch workflow
batchRunWorkflowCmd :: ApiToken -> ApiVersion -> Workflows.BatchRunWorkflowRequest -> Client.ClientM ()
batchRunWorkflowCmd token extendApiVersion request = do
  response <- Workflows.batchRunWorkflow token extendApiVersion request
  let Workflows.BatchRunWorkflowResponse
        { Workflows.batchRunWorkflowResponseSuccess = isSuccess,
          Workflows.batchRunWorkflowResponseBatchId = batchId
        } = response
  liftIO $ do
    putStrLn $ "Success: " ++ show isSuccess
    putStrLn $ "Batch ID: " ++ unpack batchId
    putStrLn "Workflow runs have been queued for processing."
    putStrLn "To check the status of your batch workflow runs, use:"
    putStrLn $ "  extend-example list-runs --workflow-id " ++ unpack (Workflows.batchRunWorkflowRequestWorkflowId request)
    putStrLn "You can filter the results by looking for the batch ID in the output:"
    putStrLn $ "  Batch ID: " ++ unpack batchId

-- | Command to create a workflow
createWorkflowCmd :: ApiToken -> ApiVersion -> Workflows.CreateWorkflowRequest -> Client.ClientM ()
createWorkflowCmd token extendApiVersion request = do
  response <- Workflows.createWorkflow token extendApiVersion request
  let Workflows.CreateWorkflowResponse
        { Workflows.createWorkflowResponseSuccess = isSuccess,
          Workflows.createWorkflowResponseWorkflow = workflow
        } = response
  liftIO $ do
    putStrLn $ "Success: " ++ show isSuccess
    putStrLn $ "Created workflow:"
    putStrLn $ "  - ID: " ++ unpack (Workflows.createdWorkflowObjectId workflow)
    putStrLn $ "  - Name: " ++ unpack (Workflows.createdWorkflowObjectName workflow)
    putStrLn $ "  - Version: " ++ unpack (Workflows.createdWorkflowObjectVersion workflow)
    putStrLn $ "  - Object Type: " ++ show (Workflows.createdWorkflowObjectType workflow)

-- | CLI options
opts :: ParserInfo Command
opts =
  info
    (commandParser <**> helper)
    ( fullDesc
        <> progDesc "Extend API CLI example"
        <> header "extend-example - a CLI tool for interacting with the Extend API"
    )

-- | Create client environment
createClientEnv :: String -> IO Client.ClientEnv
createClientEnv baseUrl = do
  manager <- HTTP.newManager HTTPS.tlsManagerSettings
  pure $ Client.mkClientEnv manager (Client.BaseUrl Client.Https baseUrl 443 "")

-- | Command to run a processor
runProcessorCmd :: ApiToken -> ApiVersion -> Processors.RunProcessorRequest -> Client.ClientM ()
runProcessorCmd token version request = do
  response <- Processors.runProcessor token version request
  let Processors.RunProcessorResponse
        { Processors.runProcessorResponseSuccess = success,
          Processors.runProcessorResponseProcessorRun = processorRun
        } = response
  liftIO $ do
    putStrLn $ "Success: " ++ show success
    printProcessorRun processorRun

-- | Command to get a processor run
getProcessorRunCmd :: ApiToken -> ApiVersion -> Text -> Client.ClientM ()
getProcessorRunCmd token version runId = do
  response <- Processors.getProcessorRun token version runId
  let Processors.GetProcessorRunResponse
        { Processors.getProcessorRunResponseSuccess = success,
          Processors.getProcessorRunResponseProcessorRun = processorRun
        } = response
  liftIO $ do
    putStrLn $ "Success: " ++ show success
    printProcessorRun processorRun

-- | Command to create a processor
createProcessorCmd :: ApiToken -> ApiVersion -> Processors.CreateProcessorRequest -> Client.ClientM ()
createProcessorCmd token version request = do
  response <- Processors.createProcessor token version request
  let Processors.CreateProcessorResponse
        { Processors.createProcessorResponseSuccess = success,
          Processors.createProcessorResponseProcessor = processor
        } = response
  liftIO $ do
    putStrLn $ "Success: " ++ show success
    printProcessor processor

-- | Command to update a processor
updateProcessorCmd :: ApiToken -> ApiVersion -> Text -> Processors.UpdateProcessorRequest -> Client.ClientM ()
updateProcessorCmd token version processorId request = do
  response <- Processors.updateProcessor token version processorId request
  let Processors.UpdateProcessorResponse
        { Processors.updateProcessorResponseSuccess = success,
          Processors.updateProcessorResponseProcessor = processor
        } = response
  liftIO $ do
    putStrLn $ "Success: " ++ show success
    printProcessor processor

-- | Command to get a processor version
getProcessorVersionCmd :: ApiToken -> ApiVersion -> Text -> Text -> Client.ClientM ()
getProcessorVersionCmd token version processorId versionId = do
  response <- Processors.getProcessorVersion token version processorId versionId
  let Processors.GetProcessorVersionResponse
        { Processors.getProcessorVersionResponseSuccess = success,
          Processors.getProcessorVersionResponseVersion = processorVersion
        } = response
  liftIO $ do
    putStrLn $ "Success: " ++ show success
    printProcessorVersion processorVersion

-- | Command to list processor versions
listProcessorVersionsCmd :: ApiToken -> ApiVersion -> Text -> Client.ClientM ()
listProcessorVersionsCmd token version processorId = do
  response <- Processors.listProcessorVersions token version processorId
  let Processors.ListProcessorVersionsResponse
        { Processors.listProcessorVersionsResponseSuccess = success,
          Processors.listProcessorVersionsResponseVersions = versions
        } = response
  liftIO $ do
    putStrLn $ "Success: " ++ show success
    putStrLn $ "Found " ++ show (length versions) ++ " processor versions:"
    mapM_ printProcessorVersion versions

-- | Command to publish a processor version
publishProcessorVersionCmd :: ApiToken -> ApiVersion -> Text -> Processors.PublishProcessorVersionRequest -> Client.ClientM ()
publishProcessorVersionCmd token version processorId request = do
  response <- Processors.publishProcessorVersion token version processorId request
  let Processors.PublishProcessorVersionResponse
        { Processors.publishProcessorVersionResponseSuccess = success,
          Processors.publishProcessorVersionResponseProcessorVersion = processorVersion
        } = response
  liftIO $ do
    putStrLn $ "Success: " ++ show success
    printProcessorVersion processorVersion

-- | Command to get a batch processor run
getBatchProcessorRunCmd :: ApiToken -> ApiVersion -> Text -> Client.ClientM ()
getBatchProcessorRunCmd token version batchRunId = do
  response <- Processors.getBatchProcessorRun token version batchRunId
  let Processors.GetBatchProcessorRunResponse
        { Processors.getBatchProcessorRunResponseSuccess = success,
          Processors.getBatchProcessorRunResponseBatchProcessorRun = batchProcessorRun
        } = response
  liftIO $ do
    putStrLn $ "Success: " ++ show success
    printBatchProcessorRun batchProcessorRun

-- | Helper to print a processor
printProcessor :: Processors.Processor -> IO ()
printProcessor processor = do
  putStrLn $ "  - ID: " ++ unpack (Processors.processorId processor)
  putStrLn $ "    Name: " ++ unpack (Processors.processorName processor)
  putStrLn $ "    Type: " ++ show (Processors.processorType processor)
  putStrLn $ "    Created At: " ++ show (Processors.processorCreatedAt processor)
  putStrLn $ "    Updated At: " ++ show (Processors.processorUpdatedAt processor)
  case Processors.processorDraftVersion processor of
    Just draftVersion -> do
      putStrLn "    Draft Version:"
      putStrLn $ "      ID: " ++ unpack (Processors.processorVersionId draftVersion)
      putStrLn $ "      Version: " ++ unpack (Processors.processorVersionVersion draftVersion)
    Nothing -> putStrLn "    No Draft Version"
  putStrLn ""

-- | Helper to print a processor version
printProcessorVersion :: Processors.ProcessorVersion -> IO ()
printProcessorVersion version = do
  putStrLn $ "  - ID: " ++ unpack (Processors.processorVersionId version)
  putStrLn $ "    Processor ID: " ++ unpack (Processors.processorVersionProcessorId version)
  case Processors.processorVersionProcessorName version of
    Just name -> putStrLn $ "    Processor Name: " ++ unpack name
    Nothing -> pure ()
  putStrLn $ "    Version: " ++ unpack (Processors.processorVersionVersion version)
  putStrLn $ "    Type: " ++ show (Processors.processorVersionProcessorType version)
  case Processors.processorVersionDescription version of
    Just desc -> putStrLn $ "    Description: " ++ unpack desc
    Nothing -> pure ()
  putStrLn $ "    Created At: " ++ show (Processors.processorVersionCreatedAt version)
  putStrLn $ "    Updated At: " ++ show (Processors.processorVersionUpdatedAt version)
  putStrLn ""

-- | Helper to print a processor run
printProcessorRun :: Processors.ProcessorRun -> IO ()
printProcessorRun processorRun = do
  putStrLn $ "  - ID: " ++ unpack (Processors.processorRunId processorRun)
  putStrLn $ "    Processor ID: " ++ unpack (Processors.processorRunProcessorId processorRun)
  putStrLn $ "    Processor Name: " ++ unpack (Processors.processorRunProcessorName processorRun)
  putStrLn $ "    Status: " ++ show (Processors.processorRunStatus processorRun)
  case Processors.processorRunType processorRun of
    Just type_ -> putStrLn $ "    Type: " ++ unpack type_
    Nothing -> pure ()
  case Processors.processorRunUrl processorRun of
    Just url -> putStrLn $ "    URL: " ++ unpack url
    Nothing -> pure ()
  putStrLn $ "    Reviewed: " ++ show (Processors.processorRunReviewed processorRun)
  putStrLn $ "    Edited: " ++ show (Processors.processorRunEdited processorRun)
  putStrLn $ "    Files: " ++ show (length (Processors.processorRunFiles processorRun))
  case Processors.processorRunFailureReason processorRun of
    Just reason -> putStrLn $ "    Failure Reason: " ++ unpack reason
    Nothing -> pure ()
  case Processors.processorRunFailureMessage processorRun of
    Just message -> putStrLn $ "    Failure Message: " ++ unpack message
    Nothing -> pure ()
  putStrLn ""

-- | Helper to print a batch processor run
printBatchProcessorRun :: Processors.BatchProcessorRun -> IO ()
printBatchProcessorRun batchRun = do
  putStrLn $ "  - ID: " ++ unpack (Processors.batchProcessorRunId batchRun)
  putStrLn $ "    Processor ID: " ++ unpack (Processors.batchProcessorRunProcessorId batchRun)
  putStrLn $ "    Processor Name: " ++ unpack (Processors.batchProcessorRunProcessorName batchRun)
  putStrLn $ "    Status: " ++ unpack (Processors.batchProcessorRunStatus batchRun)
  putStrLn $ "    Source: " ++ unpack (Processors.batchProcessorRunSource batchRun)
  case Processors.batchProcessorRunSourceId batchRun of
    Just sourceId -> putStrLn $ "    Source ID: " ++ unpack sourceId
    Nothing -> pure ()
  putStrLn $ "    Run Count: " ++ show (Processors.batchProcessorRunRunCount batchRun)
  putStrLn $ "    Created At: " ++ show (Processors.batchProcessorRunCreatedAt batchRun)
  putStrLn $ "    Updated At: " ++ show (Processors.batchProcessorRunUpdatedAt batchRun)
  putStrLn ""