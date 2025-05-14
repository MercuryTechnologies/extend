{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson (Result (Error, Success), fromJSON, toJSON)
import qualified Data.Aeson as Aeson
import Data.ByteString.Lazy.Char8 (pack)
import Data.Text (Text)
import Data.Time.Calendar (Day (..))
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)
import Extend.V1
import qualified Extend.V1.Workflows as W
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Extend API Tests"
    [ testCase "ApiToken construction" $
        let token = ApiToken "test-token"
         in unApiToken token @?= "test-token",
      testCase "ApiVersion construction" $
        let version = ApiVersion "2025-04-21"
         in unApiVersion version @?= "2025-04-21",
      testCase "defaultApiVersion" $
        unApiVersion defaultApiVersion @?= "2025-04-21",
      testGroup
        "Object Types"
        [ testCase "WorkflowObject JSON round trip" $
            let obj = WorkflowObject
                json = toJSON obj
                parsed = fromJSON json
             in parsed @?= Success WorkflowObject,
          testCase "FileObject JSON round trip" $
            let obj = FileObject
                json = toJSON obj
                parsed = fromJSON json
             in parsed @?= Success FileObject
        ],
      testGroup
        "RunWorkflowRequest"
        [ testCase "Basic request serialization" $
            let request =
                  W.RunWorkflowRequest
                    "wf_test" -- workflowId
                    Nothing -- files
                    Nothing -- rawTexts
                    Nothing -- priority
                    Nothing -- metadata
                    Nothing -- version
                json = toJSON request
             in case fromJSON json of
                  Success r ->
                    let W.RunWorkflowRequest wfId _ _ _ _ _ = r
                     in wfId @?= "wf_test"
                  Error err -> assertFailure $ "Failed to parse: " ++ err
        ],
      testGroup
        "GetWorkflowRunResponse"
        [ testCase "Basic response deserialization" $
            let jsonStr = "{\"success\":true,\"workflowRun\":{\"object\":\"workflow_run\",\"id\":\"workflow_run_test\",\"status\":\"PROCESSED\",\"reviewed\":false}}"
                mValue = Aeson.decode (pack jsonStr)
             in case mValue of
                  Nothing -> assertFailure "Failed to decode JSON string"
                  Just value -> case fromJSON value of
                    Success r ->
                      do
                        W.getWorkflowRunResponseSuccess r @?= True
                        W.workflowRunId (W.getWorkflowRunResponseWorkflowRun r) @?= "workflow_run_test"
                        W.workflowRunStatus (W.getWorkflowRunResponseWorkflowRun r) @?= W.Processed
                    Error err -> assertFailure $ "Failed to parse: " ++ err,
          testCase "Response serialization/deserialization round trip" $
            let testTime = UTCTime (ModifiedJulianDay 59000) (secondsToDiffTime 0)
                workflowRun =
                  W.WorkflowRun
                    { W.workflowRunObject = WorkflowRunObject,
                      W.workflowRunId = "workflow_run_test",
                      W.workflowRunName = Just "Test Workflow Run",
                      W.workflowRunUrl = Just "https://example.com/workflow_run",
                      W.workflowRunStatus = W.Processed,
                      W.workflowRunMetadata = Nothing,
                      W.workflowRunFiles = Nothing,
                      W.workflowRunWorkflowId = Just "workflow_test",
                      W.workflowRunWorkflowName = Just "Test Workflow",
                      W.workflowRunWorkflowVersionId = Just "workflow_version_test",
                      W.workflowRunCreatedAt = Just testTime,
                      W.workflowRunUpdatedAt = Just testTime,
                      W.workflowRunInitialRunAt = Just testTime,
                      W.workflowRunReviewed = False,
                      W.workflowRunReviewedBy = Nothing,
                      W.workflowRunReviewedAt = Nothing,
                      W.workflowRunStartTime = Just testTime,
                      W.workflowRunEndTime = Just testTime,
                      W.workflowRunOutputs = Nothing,
                      W.workflowRunStepRuns = Nothing,
                      W.workflowRunWorkflow = Nothing,
                      W.workflowRunBatchId = Nothing,
                      W.workflowRunFailureReason = Nothing,
                      W.workflowRunFailureMessage = Nothing,
                      W.workflowRunRejectionNote = Nothing
                    }
                response = W.GetWorkflowRunResponse True workflowRun
                json = toJSON response
             in case fromJSON json of
                  Success r -> r @?= response
                  Error err -> assertFailure $ "Round trip failed: " ++ err
        ]
    ]