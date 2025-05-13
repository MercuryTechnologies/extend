{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Example usage of the Extend API with command-line argument parsing
module Main (main) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (isJust)
import Data.Text (Text, pack, unpack)
import Extend.V1
import qualified Extend.V1.Workflows as Workflows
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTPS
import Options.Applicative
import qualified Servant.Client as Client
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.IO (hPrint, hPutStrLn, stderr)

-- | CLI command options
data Command
  = GetWorkflow Text
  | RunWorkflow Text [FilePath]
  | ListWorkflowRuns (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Int)
  | GetWorkflowRun Text
  deriving (Show)

-- | Parse CLI arguments
commandParser :: Parser Command
commandParser =
  subparser $
    mconcat
      [ command
          "get-workflow"
          ( info
              (GetWorkflow <$> workflowIdArg)
              (progDesc "Get a specific workflow by ID")
          ),
        command
          "run-workflow"
          ( info
              ( RunWorkflow
                  <$> workflowIdArg
                  <*> many (strArgument (metavar "FILE" <> help "File path to process"))
              )
              (progDesc "Run a workflow with specified files")
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

-- | Main entry point
main :: IO ()
main = do
  command <- execParser opts

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
      version = defaultApiVersion

  result <- runCommand token version env command
  case result of
    Left err -> do
      hPutStrLn stderr "API Error:"
      hPrint stderr err
      exitFailure
    Right () -> pure ()

-- | Run the appropriate command
runCommand :: ApiToken -> ApiVersion -> Client.ClientEnv -> Command -> IO (Either Client.ClientError ())
runCommand token version env = \case
  GetWorkflow workflowId -> do
    putStrLn $ "Getting workflow: " ++ unpack workflowId
    Client.runClientM (getWorkflowCmd token version workflowId) env
  RunWorkflow workflowId filePaths -> do
    putStrLn $ "Running workflow: " ++ unpack workflowId
    putStrLn $ "Files: " ++ show filePaths
    when (null filePaths) $
      putStrLn "Warning: No files specified. This will create an empty workflow run."
    -- TODO: Implement file handling
    let request =
          Workflows.RunWorkflowRequest
            { Workflows.runWorkflowRequestWorkflowId = workflowId,
              Workflows.runWorkflowRequestFiles = Nothing, -- Would need to prepare files
              Workflows.runWorkflowRequestRawTexts = Nothing,
              Workflows.runWorkflowRequestPriority = Nothing,
              Workflows.runWorkflowRequestMetadata = Nothing
            }
    Client.runClientM (runWorkflowCmd token version request) env
  ListWorkflowRuns maybeWorkflowId maybeStatus maybeFileNameContains maybeSortBy maybeSortDir maybeNextPageToken maybeLimit -> do
    putStrLn "Listing workflow runs..."

    -- Show filter information if present
    when (isJust maybeWorkflowId) $
      putStrLn $
        "Filtered by workflow ID: " ++ maybe "" unpack maybeWorkflowId
    when (isJust maybeStatus) $
      putStrLn $
        "Filtered by status: " ++ maybe "" unpack maybeStatus
    when (isJust maybeFileNameContains) $
      putStrLn $
        "Filtered by filename containing: " ++ maybe "" unpack maybeFileNameContains
    when (isJust maybeSortBy) $
      putStrLn $
        "Sorted by: " ++ maybe "" unpack maybeSortBy
    when (isJust maybeSortDir) $
      putStrLn $
        "Sort direction: " ++ maybe "" unpack maybeSortDir
    when (isJust maybeNextPageToken) $
      putStrLn $
        "Using page token: " ++ maybe "" unpack maybeNextPageToken
    when (isJust maybeLimit) $
      putStrLn $
        "Limited to: " ++ maybe "" show maybeLimit ++ " results"

    Client.runClientM (listWorkflowRunsCmd token version maybeWorkflowId maybeStatus maybeFileNameContains maybeSortBy maybeSortDir maybeNextPageToken maybeLimit) env
  GetWorkflowRun runId -> do
    putStrLn $ "Getting workflow run: " ++ unpack runId
    Client.runClientM (getWorkflowRunCmd token version runId) env

-- | Command to get a specific workflow
getWorkflowCmd :: ApiToken -> ApiVersion -> Text -> Client.ClientM ()
getWorkflowCmd token version workflowId = do
  response <- getWorkflow token version workflowId
  let Workflows.GetWorkflowResponse
        { Workflows.getWorkflowResponseSuccess = success,
          Workflows.getWorkflowResponseWorkflow = workflow
        } = response
  liftIO $ do
    putStrLn $ "Success: " ++ show success
    putStrLn $ "Workflow ID: " ++ unpack (Workflows.workflowId workflow)
    putStrLn $ "Name: " ++ unpack (Workflows.workflowName workflow)
    putStrLn $ "Version: " ++ unpack (Workflows.workflowVersion workflow)
    putStrLn $ "Created at: " ++ show (Workflows.workflowCreatedAt workflow)
    putStrLn $ "Updated at: " ++ show (Workflows.workflowUpdatedAt workflow)
    putStrLn $ "Description: " ++ maybe "(none)" unpack (Workflows.workflowDescription workflow)

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
    mapM_ (\run -> putStrLn $ "  - " ++ unpack (Workflows.workflowRunId run)) workflowRuns

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
    mapM_ printWorkflowRun workflowRuns
    when (isJust nextPageToken) $
      putStrLn $
        "Next page token: " ++ maybe "" unpack nextPageToken

-- | Command to get a specific workflow run
getWorkflowRunCmd :: ApiToken -> ApiVersion -> Text -> Client.ClientM ()
getWorkflowRunCmd token version runId = do
  response <- getWorkflowRun token version runId
  let Workflows.GetWorkflowRunResponse
        { Workflows.getWorkflowRunResponseSuccess = success,
          Workflows.getWorkflowRunResponseWorkflowRun = workflowRun
        } = response
  liftIO $ do
    putStrLn $ "Success: " ++ show success
    printWorkflowRun workflowRun

-- | Helper to print a workflow run
printWorkflowRun :: Workflows.WorkflowRun -> IO ()
printWorkflowRun workflowRun = do
  putStrLn $ "  - Run: " ++ unpack (Workflows.workflowRunId workflowRun)
  putStrLn $ "    Status: " ++ show (Workflows.workflowRunStatus workflowRun)
  putStrLn $ "    Workflow: " ++ unpack (Workflows.workflowRunWorkflowName workflowRun)
  putStrLn $ "    Created at: " ++ show (Workflows.workflowRunCreatedAt workflowRun)
  putStrLn $ "    Reviewed: " ++ show (Workflows.workflowRunReviewed workflowRun)
  putStrLn ""

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