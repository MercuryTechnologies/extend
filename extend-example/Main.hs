{-# LANGUAGE ScopedTypeVariables #-}

-- | Example usage of the Extend API
module Main (main) where

import Control.Exception (throwIO)
import Data.Text (Text, pack, unpack)
import Extend.V1
import qualified Extend.V1.Workflows as Workflows
import qualified Servant.Client as Client
import System.Environment (lookupEnv)
import Prelude hiding (error)
import qualified Prelude

-- | Run a simple example that calls the Extend API
main :: IO ()
main = do
  putStrLn "Running Extend API example..."

  -- Get API key from environment variable
  maybeApiKey <- lookupEnv "EXTEND_API_KEY"
  apiKey <- case maybeApiKey of
    Just key -> pure (pack key)
    Nothing -> Prelude.error "EXTEND_API_KEY environment variable not set"

  -- Base URL for the API
  let baseUrl = "api.extend.ai"

  -- Create a client environment
  env <- getClientEnv baseUrl

  -- Create API token and use default version
  let token = ApiToken apiKey
      version = defaultApiVersion

  -- Example: List workflow runs
  putStrLn "Listing workflow runs..."
  workflowRunsResult <- Client.runClientM (listWorkflowRuns token version Nothing Nothing Nothing) env

  case workflowRunsResult of
    Left err -> do
      putStrLn "Error listing workflow runs:"
      print err
    Right
      ( Workflows.ListWorkflowRunsResponse
          listWorkflowRunsResponseSuccess
          listWorkflowRunsResponseWorkflowRuns
          _
        ) -> do
        putStrLn $ "Found " ++ show (length listWorkflowRunsResponseWorkflowRuns) ++ " workflow runs"
        mapM_
          ( \run@Workflows.WorkflowRun
               { Workflows.workflowRunId = runId,
                 Workflows.workflowRunStatus = runStatus,
                 Workflows.workflowRunWorkflowName = wfName,
                 Workflows.workflowRunCreatedAt = created
               } -> do
                putStrLn $ "  - Workflow run: " ++ unpack runId
                putStrLn $ "    Status: " ++ show runStatus
                putStrLn $ "    Workflow: " ++ unpack wfName
                putStrLn $ "    Created at: " ++ show created
                putStrLn ""
          )
          listWorkflowRunsResponseWorkflowRuns

  -- Example: Run a workflow (if needed)
  -- Uncomment and modify this section to run a workflow
  {-
  putStrLn "Running a workflow..."

  -- Create the request to run a workflow
  let runRequest = RunWorkflowRequest
          { runWorkflowRequestWorkflowId = "wf_your_workflow_id"
          , runWorkflowRequestFiles = Just
              [ ExtendFile
                  { extendFileFileName = Just "example.pdf"
                  , extendFileFileUrl = Just "https://example.com/file.pdf"
                  , extendFileFileId = Nothing
                  , extendFileOutputs = Nothing
                  }
              ]
          , runWorkflowRequestRawTexts = Nothing
          , runWorkflowRequestPriority = Just 50
          , runWorkflowRequestMetadata = Nothing
          }

  runResult <- Client.runClientM (runWorkflow token version runRequest) env

  case runResult of
      Left err -> do
          putStrLn "Error running workflow:"
          print err
      Right (RunWorkflowResponse success workflowRuns) -> do
          putStrLn $ "Success: " ++ show success
          putStrLn $ "Created " ++ show (length workflowRuns) ++ " workflow runs"
          mapM_ (\_ -> putStrLn "  - Workflow run") workflowRuns
  -}

  putStrLn "Example completed."