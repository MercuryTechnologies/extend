# Extend

This package provides comprehensive and type-safe bindings to the [Extend AI](https://docs.extend.ai/developers/authentication) API, providing both a Servant interface and non-Servant interface for convenience.

## Overview

Extend is a document processing and workflow automation platform. This library provides a Haskell client for interacting with the Extend API.

## Installation

Add the package to your dependencies:

```cabal
build-depends:
    extend-ai
```

## Usage

### Authentication

To interact with the Extend API, you need an API token:

```haskell
import Extend.V1

-- Create an API token
let token = ApiToken "your-api-key"

-- Use the default API version (2025-04-21) or specify a custom one
let version = defaultApiVersion
-- let version = ApiVersion "2025-01-01"
```

### Creating a Client Environment

Create a client environment for making API requests:

```haskell
import Extend.V1

main :: IO ()
main = do
  -- Create a client environment for the Extend API
  env <- getClientEnv "api.extend.ai"

  -- Use the environment to make API requests
  -- ...
```

### Running Workflows

Workflows are the primary way to process documents in Extend:

```haskell
import Extend.V1
import qualified Extend.V1.Workflows as Workflows
import Control.Exception (throwIO)
import Servant.Client (runClientM)
import Data.Text (unpack)

runExtendWorkflow :: IO ()
runExtendWorkflow = do
  -- Create client environment
  env <- getClientEnv "api.extend.ai"

  -- Define API token and version
  let token = ApiToken "your-api-key"
      version = defaultApiVersion

  -- Create a request to run a workflow
  let request = RunWorkflowRequest
        { runWorkflowRequestWorkflowId = "wf_123456789"
        , runWorkflowRequestFiles = Just
            [ ExtendFile
                { extendFileName = Just "example.pdf"
                , extendFileUrl = Just "https://example.com/file.pdf"
                , extendFileId = Nothing
                , extendFileOutputs = Nothing
                }
            ]
        , runWorkflowRequestRawTexts = Nothing
        , runWorkflowRequestPriority = Just 50
        , runWorkflowRequestMetadata = Just
            [("customer_id", String "cust_123"),
             ("reference_number", String "REF-456")]
        , runWorkflowRequestVersion = Nothing
        }

  -- Run the workflow
  result <- runClientM (runWorkflow token version request) env

  case result of
    Left err -> throwIO err
    Right response -> do
      -- Process the workflow runs
      let runs = runWorkflowResponseWorkflowRuns response
      mapM_ processWorkflowRun runs

-- Process a workflow run
processWorkflowRun :: Workflows.CreatedWorkflowRun -> IO ()
processWorkflowRun run = do
  -- Access fields of the WorkflowRun
  putStrLn $ "Workflow Run ID: " ++ unpack (Workflows.createdWorkflowRunId run)
  putStrLn $ "Status: " ++ show (Workflows.createdWorkflowRunStatus run)
  putStrLn $ "Reviewed: " ++ show (Workflows.createdWorkflowRunReviewed run)

  -- Access workflow information
  case Workflows.createdWorkflowRunWorkflow run of
    Just workflow -> do
      putStrLn $ "Workflow: " ++ unpack (Workflows.workflowSummaryName workflow)
      putStrLn $ "Workflow ID: " ++ unpack (Workflows.workflowSummaryId workflow)
    Nothing -> pure ()

  -- Access initial run time if available
  case Workflows.createdWorkflowRunInitialRunAt run of
    Just initialRunAt -> putStrLn $ "Initial run at: " ++ show initialRunAt
    Nothing -> pure ()

  -- Access metadata if available
  case Workflows.createdWorkflowRunMetadata run of
    Just metadata -> do
      putStrLn "Metadata:"
      mapM_ (\(key, value) -> putStrLn $ "  " ++ unpack key ++ ": " ++ show value) (toList metadata)
    Nothing -> pure ()

  -- Access batch ID if available
  case Workflows.createdWorkflowRunBatchId run of
    Just batchId -> putStrLn $ "Batch ID: " ++ unpack batchId
    Nothing -> pure ()

  -- Process document processor runs if available
  case Workflows.createdWorkflowRunOutputs run of
    Just outputs -> do
      putStrLn $ "Processor outputs: " ++ show (length outputs)
      mapM_ processProcessorOutput outputs
    Nothing -> pure ()

-- Process a document processor output
processProcessorOutput :: Workflows.DocumentProcessorRun -> IO ()
processProcessorOutput output = do
  putStrLn $ "  Processor: " ++ unpack (Workflows.documentProcessorRunProcessorName output)
  putStrLn $ "  Status: " ++ unpack (Workflows.documentProcessorRunStatus output)
  putStrLn $ "  Reviewed: " ++ show (Workflows.documentProcessorRunReviewed output)
  putStrLn $ "  Edited: " ++ show (Workflows.documentProcessorRunEdited output)
```

### Listing Workflow Runs

You can list existing workflow runs:

```haskell
import Extend.V1
import qualified Extend.V1.Workflows as Workflows
import Control.Exception (throwIO)
import Servant.Client (runClientM)
import Data.Text (unpack)

listWorkflowRunsExample :: IO ()
listWorkflowRunsExample = do
  -- Create client environment
  env <- getClientEnv "api.extend.ai"

  -- Define API token and version
  let token = ApiToken "your-api-key"
      version = defaultApiVersion

  -- List workflow runs
  result <- runClientM (listWorkflowRuns token version Nothing Nothing Nothing Nothing Nothing Nothing Nothing) env

  case result of
    Left err -> throwIO err
    Right response -> do
      let workflowRuns = listWorkflowRunsResponseWorkflowRuns response
          nextPageToken = listWorkflowRunsResponseNextPageToken response

      putStrLn $ "Found " ++ show (length workflowRuns) ++ " workflow runs"

      -- Process each workflow run
      mapM_ (\run -> do
          putStrLn $ "Run ID: " ++ unpack (Workflows.workflowRunSummaryId run)
          putStrLn $ "Status: " ++ show (Workflows.workflowRunSummaryStatus run)
          putStrLn $ "Workflow: " ++ unpack (Workflows.workflowRunSummaryWorkflowName run)
          putStrLn $ "Created at: " ++ show (Workflows.workflowRunSummaryCreatedAt run)
          putStrLn ""
        ) workflowRuns

      -- Handle pagination if more results are available
      case nextPageToken of
        Just token -> putStrLn $ "More results available with token: " ++ unpack token
        Nothing -> putStrLn "No more results available"
```

### Working with Files

You can manage files in the Extend platform:

```haskell
import Extend.V1
import qualified Extend.V1.Files as Files
import Control.Exception (throwIO)
import Servant.Client (runClientM)

uploadFileExample :: IO ()
uploadFileExample = do
  -- Create client environment
  env <- getClientEnv "api.extend.ai"

  -- Define API token and version
  let token = ApiToken "your-api-key"
      version = defaultApiVersion

  -- Create a file
  let createRequest = CreateFileRequest
        { name = "example.pdf"
        , type_ = Just "application/pdf"
        }

  result <- runClientM (createFile token version createRequest) env

  case result of
    Left err -> throwIO err
    Right response -> do
      let fileId = Files.id (Files.file (data_ response))

      -- Upload file content
      let fileContent = "file content as text" -- Or binary data encoded as Text
      uploadResult <- runClientM (uploadFile token version fileId fileContent) env

      case uploadResult of
        Left err -> throwIO err
        Right uploadResponse -> do
          -- File uploaded successfully
          putStrLn $ "File uploaded: " ++ show (success uploadResponse)

      -- Get file information
      getFileResult <- runClientM (getFile token version fileId) env

      case getFileResult of
        Left err -> throwIO err
        Right getFileResponse -> do
          let file = Files.file (data_ getFileResponse)
          putStrLn $ "File name: " ++ unpack (Files.name file)
          putStrLn $ "File type: " ++ unpack (fromMaybe "" (Files.type_ file))

          -- Access file contents if available
          case Files.contents file of
            Just contents -> do
              case Files.rawText contents of
                Just text -> putStrLn $ "Raw text: " ++ unpack text
                Nothing -> pure ()

              -- Access pages if available
              case Files.pages contents of
                Just pages -> putStrLn $ "Number of pages: " ++ show (length pages)
                Nothing -> pure ()

            Nothing -> pure ()
```

### Running Processors

Processors are individual document processing components:

```haskell
import Extend.V1
import qualified Extend.V1.Processors as Processors
import Control.Exception (throwIO)
import Servant.Client (runClientM)
import Data.Aeson (object, (.=))

runProcessorExample :: IO ()
runProcessorExample = do
  -- Create client environment
  env <- getClientEnv "api.extend.ai"

  -- Define API token and version
  let token = ApiToken "your-api-key"
      version = defaultApiVersion

  -- Create a request to run a processor
  let request = RunProcessorRequest
        { fileIds = ["file_123456789"]
        , config = Just (ProcessorConfig (object [
              "language" .= ("english" :: Text),
              "extractFields" .= True
            ]))
        }

  -- Run the processor
  result <- runClientM (runProcessor token version "dp_123456789" request) env

  case result of
    Left err -> throwIO err
    Right response -> do
      -- Process the processor run
      let run = Processors.processorRun (data_ response)
      putStrLn $ "Processor run ID: " ++ unpack (Processors.id run)
      putStrLn $ "Status: " ++ show (Processors.status run)
      putStrLn $ "Processor: " ++ unpack (Processors.processorName run)
      putStrLn $ "Created at: " ++ show (Processors.createdAt run)

      -- Access the output
      putStrLn $ "Output: " ++ show (Processors.output run)

      -- Process files
      let files = Processors.files run
      putStrLn $ "Number of files: " ++ show (length files)

      -- Check for failures
      case Processors.failureReason run of
        Just reason -> putStrLn $ "Failure reason: " ++ unpack reason
        Nothing -> pure ()
```

## Error Handling

The library provides structured error handling through the `ClientError` type from Servant:

```haskell
import Extend.V1
import Control.Exception (catch, throwIO)
import Servant.Client (runClientM, ClientError(..))
import Data.Text (unpack)

errorHandlingExample :: IO ()
errorHandlingExample = do
  -- Create client environment
  env <- getClientEnv "api.extend.ai"

  -- Define API token and version
  let token = ApiToken "your-api-key"
      version = defaultApiVersion

  -- Create a request to run a workflow
  let request = RunWorkflowRequest
        { runWorkflowRequestWorkflowId = "wf_123456789"
        , runWorkflowRequestFiles = Nothing
        , runWorkflowRequestRawTexts = Nothing
        , runWorkflowRequestPriority = Nothing
        , runWorkflowRequestMetadata = Nothing
        , runWorkflowRequestVersion = Nothing
        }

  -- Run the workflow with error handling
  result <- runClientM (runWorkflow token version request) env

  case result of
    Left err -> do
      putStrLn "API call failed:"
      case err of
        FailureResponse _ response -> do
          putStrLn $ "Status code: " ++ show (responseStatusCode response)
          putStrLn $ "Response body: " ++ show (responseBody response)
        ConnectionError e ->
          putStrLn $ "Connection error: " ++ show e
        _ ->
          putStrLn $ "Other error: " ++ show err

    Right response -> do
      -- Process the workflow runs
      let runs = runWorkflowResponseWorkflowRuns response
      -- ...
```

## Documentation

For more details about the Extend API, refer to the [official documentation](https://docs.extend.ai/developers).

## License

This package is licensed under the BSD-3-Clause license.
