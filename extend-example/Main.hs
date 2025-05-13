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
    Right (Workflows.ListWorkflowRunsResponse _ workflowRuns _) -> do
      putStrLn $ "Found " ++ show (length workflowRuns) ++ " workflow runs"
      mapM_
        ( \run -> do
            let runId = case run of
                  Workflows.WorkflowRun _ id status workflowId workflowName _ createdAt _ _ _ _ _ _ _ _ _ -> id
            putStrLn $ "  - Workflow run: " ++ unpack runId
            putStrLn $ "    Status: " ++ show (case run of Workflows.WorkflowRun _ _ status _ _ _ _ _ _ _ _ _ _ _ _ _ -> status)
            putStrLn $ "    Workflow: " ++ unpack (case run of Workflows.WorkflowRun _ _ _ _ workflowName _ _ _ _ _ _ _ _ _ _ _ -> workflowName)
            putStrLn $ "    Created at: " ++ show (case run of Workflows.WorkflowRun _ _ _ _ _ _ createdAt _ _ _ _ _ _ _ _ _ -> createdAt)
            putStrLn ""
        )
        workflowRuns

  -- Example: Run a workflow (if needed)
  -- Uncomment and modify this section to run a workflow
  {-
  putStrLn "Running a workflow..."

  -- Create the request to run a workflow
  let runRequest = RunWorkflowRequest
          { workflowId = "wf_your_workflow_id"
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

  runResult <- Client.runClientM (runWorkflow token version runRequest) env

  case runResult of
      Left err -> do
          putStrLn "Error running workflow:"
          print err
      Right (SuccessResponse success response) -> do
          putStrLn $ "Success: " ++ show success
          putStrLn $ "Created " ++ show (length (workflowRuns response)) ++ " workflow runs"
          mapM_ (\_ -> putStrLn "  - Workflow run") (workflowRuns response)
  -}

  putStrLn "Example completed."