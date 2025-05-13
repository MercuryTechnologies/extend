# Extend

This package provides comprehensive and type-safe bindings to the [Extend AI](https://docs.extend.ai/developers/authentication) API, providing both a Servant interface and non-Servant interface for convenience.

## Overview

Extend is a document processing and workflow automation platform. This library provides a Haskell client for interacting with the Extend API.

## Installation

Add the package to your dependencies:

```cabal
build-depends:
    extend
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
import Control.Exception (throwIO)
import Servant.Client (runClientM)

runExtendWorkflow :: IO ()
runExtendWorkflow = do
  -- Create client environment
  env <- getClientEnv "api.extend.ai"

  -- Define API token and version
  let token = ApiToken "your-api-key"
      version = defaultApiVersion

  -- Create a request to run a workflow
  let request = RunWorkflowRequest
        { workflowId = "wf_123456789"
        , files = Just
            [ ExtendFile
                { fileName = Just "example.pdf"
                , fileUrl = Just "https://example.com/file.pdf"
                , fileId = Nothing
                , outputs = Nothing
                }
            ]
        , rawTexts = Nothing
        , priority = Just 50
        , metadata = Nothing
        }

  -- Run the workflow
  result <- runClientM (runWorkflow token version request) env

  case result of
    Left err -> throwIO err
    Right (SuccessResponse _ response) -> do
      -- Process the workflow runs
      let runs = workflowRuns response
      -- ...
```

### Working with Files

You can manage files in the Extend platform:

```haskell
import Extend.V1
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
    Right (SuccessResponse _ response) -> do
      let fileId = id (file response)

      -- Upload file content
      let fileContent = "file content as text" -- Or binary data encoded as Text
      uploadResult <- runClientM (uploadFile token version fileId fileContent) env

      case uploadResult of
        Left err -> throwIO err
        Right response -> do
          -- File uploaded successfully
          -- ...
```

### Running Processors

Processors are individual document processing components:

```haskell
import Extend.V1
import Control.Exception (throwIO)
import Servant.Client (runClientM)

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
        , config = Nothing
        }

  -- Run the processor
  result <- runClientM (runProcessor token version "dp_123456789" request) env

  case result of
    Left err -> throwIO err
    Right (SuccessResponse _ response) -> do
      -- Process the processor run
      let run = processorRun response
      -- ...
```

## Error Handling

The library provides structured error handling through the `ApiError` type:

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
        { workflowId = "wf_123456789"
        , files = Nothing
        , rawTexts = Nothing
        , priority = Nothing
        , metadata = Nothing
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

    Right (SuccessResponse _ response) -> do
      -- Process the workflow runs
      let runs = workflowRuns response
      -- ...
```

## Documentation

For more details about the Extend API, refer to the [official documentation](https://docs.extend.ai/developers).

## License

This package is licensed under the BSD-3-Clause license.
