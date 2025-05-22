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

### Working with Workflows

Extend organizes document processing around workflows. Here's how to interact with workflows:

```haskell
import Extend.V1
import qualified Extend.V1.Workflows as Workflows
import Control.Exception (throwIO)
import Servant.Client (runClientM)
import Data.Text (unpack)

-- Get information about a workflow
getWorkflowExample :: IO ()
getWorkflowExample = do
  env <- getClientEnv "api.extend.ai"
  let token = ApiToken "your-api-key"
      version = defaultApiVersion
      workflowId = "wf_123456789"

  result <- runClientM (getWorkflow token version workflowId) env

  case result of
    Left err -> throwIO err
    Right response -> do
      let workflow = successResponseData response
      putStrLn $ "Workflow name: " ++ unpack (workflowName workflow)
      putStrLn $ "Workflow version: " ++ unpack (workflowVersion workflow)
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

### Batch Running Workflows

For processing multiple files in a batch:

```haskell
import Extend.V1
import qualified Extend.V1.Workflows as Workflows
import Control.Exception (throwIO)
import Servant.Client (runClientM)
import Data.Text (unpack)

batchRunWorkflowExample :: IO ()
batchRunWorkflowExample = do
  env <- getClientEnv "api.extend.ai"
  let token = ApiToken "your-api-key"
      version = defaultApiVersion

  -- Create a batch request
  let fileInputs =
        [ BatchInputFile
            { batchInputFileName = "doc1.pdf"
            , batchInputFileUrl = "https://example.com/doc1.pdf"
            }
        , BatchInputFile
            { batchInputFileName = "doc2.pdf"
            , batchInputFileUrl = "https://example.com/doc2.pdf"
            }
        ]

      workflowInputs =
        [ BatchWorkflowInput
            { batchWorkflowInputFiles = fileInputs
            , batchWorkflowInputMetadata = Just [("batch", String "first")]
            }
        ]

      request = BatchRunWorkflowRequest
        { batchRunWorkflowRequestWorkflowId = "wf_123456789"
        , batchRunWorkflowRequestInputs = workflowInputs
        , batchRunWorkflowRequestVersion = Nothing
        }

  result <- runClientM (batchRunWorkflow token version request) env

  case result of
    Left err -> throwIO err
    Right response -> do
      putStrLn $ "Batch ID: " ++ unpack (batchRunWorkflowResponseBatchId response)
      putStrLn $ "Workflow runs created: " ++ show (length (batchRunWorkflowResponseWorkflowRuns response))
```

### Creating Workflows

You can also create new workflows:

```haskell
import Extend.V1
import qualified Extend.V1.Workflows as Workflows
import Control.Exception (throwIO)
import Servant.Client (runClientM)
import Data.Text (unpack)

createWorkflowExample :: IO ()
createWorkflowExample = do
  env <- getClientEnv "api.extend.ai"
  let token = ApiToken "your-api-key"
      version = defaultApiVersion

  let request = CreateWorkflowRequest
        { createWorkflowRequestName = "Invoice Processing"
        , createWorkflowRequestDescription = Just "Process and extract data from invoices"
        }

  result <- runClientM (createWorkflow token version request) env

  case result of
    Left err -> throwIO err
    Right response -> do
      let workflow = createdWorkflowObjectWorkflow (successResponseData response)
      putStrLn $ "Created workflow: " ++ unpack (workflowName workflow)
      putStrLn $ "ID: " ++ unpack (workflowId workflow)
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

  -- List workflow runs (with optional filtering parameters)
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

````haskell
import Extend.V1
import qualified Extend.V1.Files as Files
import Control.Exception (throwIO)
import Servant.Client (runClientM)

-- | Helper to print a file
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
      let fileId = Files.fileId (Files.createFileResponseFile (successResponseData response))

      -- Upload file content
      let fileContent = "file content as text" -- Or binary data encoded as Text
      uploadResult <- runClientM (uploadFile token version fileId fileContent) env

      case uploadResult of
        Left err -> throwIO err
        Right uploadResponse -> do
          -- File uploaded successfully
          putStrLn $ "File uploaded: " ++ show (uploadFileResponseSuccess uploadResponse)

      -- Get file information
      getFileResult <- runClientM (getFile token version fileId Nothing Nothing Nothing) env

      case getFileResult of
        Left err -> throwIO err
        Right getFileResponse -> do
          let file = Files.getFileResponseFile getFileResponse
          putStrLn $ "File name: " ++ unpack (Files.fileName file)
          putStrLn $ "File type: " ++ unpack (fromMaybe "" (Files.fileType file))

          -- Access file contents if available
          case Files.fileContents file of
            Just contents -> do
              case Files.fileContentsRawText contents of
                Just text -> putStrLn $ "Raw text: " ++ unpack text
                Nothing -> pure ()

              -- Access pages if available
              case Files.fileContentsPages contents of
                Just pages -> putStrLn $ "Number of pages: " ++ show (length pages)
                Nothing -> pure ()

            Nothing -> pure ()

### Getting File Content

You can retrieve file information with various content options:

```haskell
getFileExample :: IO ()
getFileExample = do
  env <- getClientEnv "api.extend.ai"
  let token = ApiToken "your-api-key"
      version = defaultApiVersion
      fileId = "file_AbC123XyZ"

  -- Get file with raw text content
  result <- runClientM (getFile token version fileId (Just True) Nothing Nothing) env

  case result of
    Left err -> throwIO err
    Right response -> do
      let file = Files.getFileResponseFile response
      putStrLn $ "File ID: " ++ unpack (Files.fileId file)
      putStrLn $ "File name: " ++ unpack (Files.fileName file)

      -- Access raw text content
      case Files.fileContents file of
        Just contents -> do
          case Files.fileContentsRawText contents of
            Just text -> putStrLn $ "Raw text content: " ++ unpack text
            Nothing -> putStrLn "No raw text content available"

          -- Access pages if available
          case Files.fileContentsPages contents of
            Just pages -> do
              putStrLn $ "File has " ++ show (length pages) ++ " pages"
              -- Process individual pages
              forM_ pages $ \page -> do
                putStrLn $ "Page " ++ show (Files.pageNumber page)
                case Files.pageRawText page of
                  Just text -> putStrLn $ "  Content: " ++ unpack text
                  Nothing -> putStrLn "  No text available for this page"
            Nothing -> putStrLn "No page information available"
        Nothing -> putStrLn "No file contents available"
````

When using the `getFile` function, you can specify which content to include:

- `rawText`: Set to `Just True` to include raw text content
- `markdown`: Set to `Just True` to include markdown content
- `html`: Set to `Just True` to include HTML content

### Using the CLI to Access Files

The example CLI application provides a `get-file` command for retrieving files:

```
# Retrieve basic file information
extend-example get-file file_AbC123XyZ

# Include raw text content
extend-example get-file file_AbC123XyZ --raw-text

# Include markdown content
extend-example get-file file_AbC123XyZ --markdown

# Include HTML content
extend-example get-file file_AbC123XyZ --html

# Include all content types
extend-example get-file file_AbC123XyZ --raw-text --markdown --html
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
  let fileInput = Processors.ProcessorRunFileInput
        { Processors.processorRunFileInputFileName = Just "test.pdf"
        , Processors.processorRunFileInputFileUrl = Just "https://example.com/test.pdf"
        , Processors.processorRunFileInputFileId = Nothing
        }

      request = Processors.RunProcessorRequest
        { Processors.runProcessorRequestProcessorId = "dp_123456789"
        , Processors.runProcessorRequestVersion = Just "1.0"
        , Processors.runProcessorRequestFile = Just fileInput
        , Processors.runProcessorRequestRawText = Nothing
        , Processors.runProcessorRequestPriority = Just 10
        , Processors.runProcessorRequestMetadata = Nothing
        , Processors.runProcessorRequestConfig = Nothing
        }

  -- Run the processor
  result <- runClientM (runProcessor token version request) env

  case result of
    Left err -> throwIO err
    Right response -> do
      -- Process the processor run
      let run = Processors.runProcessorResponseProcessorRun response
      putStrLn $ "Processor run ID: " ++ unpack (Processors.processorRunId run)
      putStrLn $ "Status: " ++ show (Processors.processorRunStatus run)
      putStrLn $ "Processor: " ++ unpack (Processors.processorRunProcessorName run)

      -- Access the output
      putStrLn $ "Output: " ++ show (Processors.processorRunOutput run)
```

### Getting Processor Run Information

You can retrieve information about a specific processor run:

```haskell
getProcessorRunExample :: IO ()
getProcessorRunExample = do
  env <- getClientEnv "api.extend.ai"
  let token = ApiToken "your-api-key"
      version = defaultApiVersion
      processorRunId = "dpr_123456789"

  result <- runClientM (getProcessorRun token version processorRunId) env

  case result of
    Left err -> throwIO err
    Right response -> do
      let run = Processors.getProcessorRunResponseProcessorRun response
      putStrLn $ "Processor run ID: " ++ unpack (Processors.processorRunId run)
      putStrLn $ "Status: " ++ show (Processors.processorRunStatus run)
      putStrLn $ "Reviewed: " ++ show (Processors.processorRunReviewed run)
      putStrLn $ "Edited: " ++ show (Processors.processorRunEdited run)
```

### Creating Processors

You can create new document processors:

```haskell
createProcessorExample :: IO ()
createProcessorExample = do
  env <- getClientEnv "api.extend.ai"
  let token = ApiToken "your-api-key"
      version = defaultApiVersion

  let request = Processors.CreateProcessorRequest
        { Processors.createProcessorRequestName = "Invoice Extractor"
        , Processors.createProcessorRequestType = Processors.Extract
        , Processors.createProcessorRequestCloneProcessorId = Nothing
        , Processors.createProcessorRequestConfig = Nothing
        }

  result <- runClientM (createProcessor token version request) env

  case result of
    Left err -> throwIO err
    Right response -> do
      let processor = Processors.createProcessorResponseProcessor response
      putStrLn $ "Created processor: " ++ unpack (Processors.processorName processor)
      putStrLn $ "ID: " ++ unpack (Processors.processorId processor)
      putStrLn $ "Type: " ++ show (Processors.processorType processor)
```

### Updating Processors

You can update existing document processors:

```haskell
updateProcessorExample :: IO ()
updateProcessorExample = do
  env <- getClientEnv "api.extend.ai"
  let token = ApiToken "your-api-key"
      version = defaultApiVersion
      processorId = "dp_123456789"

  let request = Processors.UpdateProcessorRequest
        { Processors.updateProcessorRequestName = Just "Updated Invoice Extractor"
        , Processors.updateProcessorRequestConfig = Nothing
        }

  result <- runClientM (updateProcessor token version processorId request) env

  case result of
    Left err -> throwIO err
    Right response -> do
      let processor = Processors.updateProcessorResponseProcessor response
      putStrLn $ "Updated processor: " ++ unpack (Processors.processorName processor)
```

### Working with Processor Versions

You can manage processor versions:

```haskell
getProcessorVersionExample :: IO ()
getProcessorVersionExample = do
  env <- getClientEnv "api.extend.ai"
  let token = ApiToken "your-api-key"
      version = defaultApiVersion
      processorId = "dp_123456789"
      versionId = "dpv_123456789"

  result <- runClientM (getProcessorVersion token version processorId versionId) env

  case result of
    Left err -> throwIO err
    Right response -> do
      let processorVersion = Processors.getProcessorVersionResponseVersion response
      putStrLn $ "Version ID: " ++ unpack (Processors.processorVersionId processorVersion)
      putStrLn $ "Version: " ++ unpack (Processors.processorVersionVersion processorVersion)
      putStrLn $ "Processor type: " ++ show (Processors.processorVersionProcessorType processorVersion)
```

### Listing Processor Versions

You can list all versions of a processor:

```haskell
listProcessorVersionsExample :: IO ()
listProcessorVersionsExample = do
  env <- getClientEnv "api.extend.ai"
  let token = ApiToken "your-api-key"
      version = defaultApiVersion
      processorId = "dp_123456789"

  result <- runClientM (listProcessorVersions token version processorId) env

  case result of
    Left err -> throwIO err
    Right response -> do
      let versions = Processors.listProcessorVersionsResponseVersions response
      putStrLn $ "Found " ++ show (length versions) ++ " versions"

      mapM_ (\v -> do
          putStrLn $ "Version ID: " ++ unpack (Processors.processorVersionId v)
          putStrLn $ "Version: " ++ unpack (Processors.processorVersionVersion v)
          putStrLn $ "Created at: " ++ show (Processors.processorVersionCreatedAt v)
          putStrLn ""
        ) versions
```

### Publishing Processor Versions

You can publish new versions of a processor:

```haskell
publishProcessorVersionExample :: IO ()
publishProcessorVersionExample = do
  env <- getClientEnv "api.extend.ai"
  let token = ApiToken "your-api-key"
      version = defaultApiVersion
      processorId = "dp_123456789"

  let request = Processors.PublishProcessorVersionRequest
        { Processors.publishProcessorVersionRequestReleaseType = "minor"
        , Processors.publishProcessorVersionRequestDescription = Just "Added new fields for extraction"
        , Processors.publishProcessorVersionRequestConfig = Nothing
        }

  result <- runClientM (publishProcessorVersion token version processorId request) env

  case result of
    Left err -> throwIO err
    Right response -> do
      let newVersion = Processors.publishProcessorVersionResponseProcessorVersion response
      putStrLn $ "Published version: " ++ unpack (Processors.processorVersionVersion newVersion)
      putStrLn $ "Version ID: " ++ unpack (Processors.processorVersionId newVersion)
```

### Batch Processor Runs

You can retrieve information about batch processor runs:

```haskell
getBatchProcessorRunExample :: IO ()
getBatchProcessorRunExample = do
  env <- getClientEnv "api.extend.ai"
  let token = ApiToken "your-api-key"
      version = defaultApiVersion
      batchRunId = "bpr_123456789"

  result <- runClientM (getBatchProcessorRun token version batchRunId) env

  case result of
    Left err -> throwIO err
    Right response -> do
      let batchRun = Processors.getBatchProcessorRunResponseBatchProcessorRun response
      putStrLn $ "Batch run ID: " ++ unpack (Processors.batchProcessorRunId batchRun)
      putStrLn $ "Processor ID: " ++ unpack (Processors.batchProcessorRunProcessorId batchRun)
      putStrLn $ "Status: " ++ unpack (Processors.batchProcessorRunStatus batchRun)
      putStrLn $ "Run count: " ++ show (Processors.batchProcessorRunRunCount batchRun)
      putStrLn $ "Source: " ++ unpack (Processors.batchProcessorRunSource batchRun)

      case Processors.batchProcessorRunSourceId batchRun of
        Just sourceId -> putStrLn $ "Source ID: " ++ unpack sourceId
        Nothing -> pure ()
```

## API Documentation

For detailed information about the Extend API, refer to the official documentation:

### Workflow API

- [Run Workflow](https://docs.extend.ai/2025-04-21/developers/api-reference/workflow-endpoints/run-workflow)
- [Get Workflow Run](https://docs.extend.ai/2025-04-21/developers/api-reference/workflow-endpoints/get-workflow-run)
- [List Workflow Runs](https://docs.extend.ai/2025-04-21/developers/api-reference/workflow-endpoints/list-workflow-runs)
- [Batch Run Workflow](https://docs.extend.ai/2025-04-21/developers/api-reference/workflow-endpoints/batch-run-workflow)
- [Create Workflow](https://docs.extend.ai/2025-04-21/developers/api-reference/workflow-endpoints/create-workflow)

### Processor API

- [Run Processor](https://docs.extend.ai/2025-04-21/developers/api-reference/processor-endpoints/run-processor)
- [Get Processor Run](https://docs.extend.ai/2025-04-21/developers/api-reference/processor-endpoints/get-processor-run)
- [Create Processor](https://docs.extend.ai/2025-04-21/developers/api-reference/processor-endpoints/create-processor)
- [Update Processor](https://docs.extend.ai/2025-04-21/developers/api-reference/processor-endpoints/update-processor)
- [Get Processor Version](https://docs.extend.ai/2025-04-21/developers/api-reference/processor-endpoints/get-processor-version)
- [List Processor Versions](https://docs.extend.ai/2025-04-21/developers/api-reference/processor-endpoints/list-processor-versions)
- [Publish Processor Version](https://docs.extend.ai/2025-04-21/developers/api-reference/processor-endpoints/publish-processor-version)
- [Get Batch Processor Run](https://docs.extend.ai/2025-04-21/developers/api-reference/processor-endpoints/get-batch-processor-run)

### File API

- [Upload File](https://docs.extend.ai/2025-04-21/developers/api-reference/file-endpoints/upload-file)
- [Get File](https://docs.extend.ai/2025-04-21/developers/api-reference/file-endpoints/get-file)

For LLM-focused documentation, check out [https://docs.extend.ai/llms.txt](https://docs.extend.ai/llms.txt) and [https://docs.extend.ai/llms-full.txt](https://docs.extend.ai/llms-full.txt).

## License

This package is licensed under the BSD-3-Clause license.
