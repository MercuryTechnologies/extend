{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson (Result (Error, Success), fromJSON, toJSON, (.=))
import qualified Data.Aeson as Aeson (object)
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
        let apiVersion = ApiVersion "2025-04-21"
         in unApiVersion apiVersion @?= "2025-04-21",
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
            let json =
                  Aeson.object
                    [ "success" .= True,
                      "workflowRun"
                        .= Aeson.object
                          [ "object" .= ("workflow_run" :: Text),
                            "id" .= ("workflow_run_test" :: Text),
                            "status" .= ("PROCESSED" :: Text),
                            "reviewed" .= False
                          ]
                    ]
             in case fromJSON json of
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
        ],
      testGroup
        "ListWorkflowRunsResponse"
        [ testCase "Basic response deserialization" $
            let json =
                  Aeson.object
                    [ "success" .= True,
                      "workflowRuns"
                        .= [ Aeson.object
                               [ "id" .= ("workflow_run_1" :: Text),
                                 "status" .= ("PROCESSED" :: Text),
                                 "workflowId" .= ("workflow_test" :: Text),
                                 "workflowName" .= ("Test Workflow" :: Text),
                                 "workflowVersionId" .= ("workflow_version_test" :: Text),
                                 "createdAt" .= ("2025-04-28T17:01:39.285Z" :: Text),
                                 "updatedAt" .= ("2025-04-28T17:01:39.285Z" :: Text),
                                 "reviewed" .= False
                               ]
                           ],
                      "nextPageToken" .= ("next_page_token_test" :: Text)
                    ]
             in case fromJSON json of
                  Success r -> do
                    W.listWorkflowRunsResponseSuccess r @?= True
                    let workflowRuns = W.listWorkflowRunsResponseWorkflowRuns r
                    length workflowRuns @?= 1
                    case workflowRuns of
                      (summary : _) -> do
                        W.workflowRunSummaryId summary @?= "workflow_run_1"
                        W.workflowRunSummaryStatus summary @?= W.Processed
                        W.workflowRunSummaryWorkflowId summary @?= "workflow_test"
                      [] -> assertFailure "Expected at least one workflow run"
                    W.listWorkflowRunsResponseNextPageToken r @?= Just "next_page_token_test"
                  Error err -> assertFailure $ "Failed to parse: " ++ err,
          testCase "Response with no nextPageToken deserialization" $
            let json =
                  Aeson.object
                    [ "success" .= True,
                      "workflowRuns"
                        .= [ Aeson.object
                               [ "id" .= ("workflow_run_1" :: Text),
                                 "status" .= ("PROCESSED" :: Text),
                                 "workflowId" .= ("workflow_test" :: Text),
                                 "workflowName" .= ("Test Workflow" :: Text),
                                 "workflowVersionId" .= ("workflow_version_test" :: Text),
                                 "createdAt" .= ("2025-04-28T17:01:39.285Z" :: Text),
                                 "updatedAt" .= ("2025-04-28T17:01:39.285Z" :: Text),
                                 "reviewed" .= False
                               ]
                           ]
                    ]
             in case fromJSON json of
                  Success r -> do
                    W.listWorkflowRunsResponseSuccess r @?= True
                    length (W.listWorkflowRunsResponseWorkflowRuns r) @?= 1
                    W.listWorkflowRunsResponseNextPageToken r @?= Nothing
                  Error err -> assertFailure $ "Failed to parse: " ++ err,
          testCase "Response serialization/deserialization round trip" $
            let testTime = UTCTime (ModifiedJulianDay 59000) (secondsToDiffTime 0)
                workflowRunSummary =
                  W.WorkflowRunSummary
                    { W.workflowRunSummaryId = "workflow_run_summary_test",
                      W.workflowRunSummaryStatus = W.Processed,
                      W.workflowRunSummaryInitialRunAt = Just testTime,
                      W.workflowRunSummaryReviewed = False,
                      W.workflowRunSummaryStartTime = Just testTime,
                      W.workflowRunSummaryWorkflowId = "workflow_test",
                      W.workflowRunSummaryWorkflowName = "Test Workflow",
                      W.workflowRunSummaryWorkflowVersionId = "workflow_version_test",
                      W.workflowRunSummaryBatchId = Just "batch_test",
                      W.workflowRunSummaryCreatedAt = testTime,
                      W.workflowRunSummaryUpdatedAt = testTime,
                      W.workflowRunSummaryName = Just "Test Workflow Run",
                      W.workflowRunSummaryUrl = Just "https://example.com/workflow_run",
                      W.workflowRunSummaryEndTime = Just testTime,
                      W.workflowRunSummaryFiles = Nothing
                    }
                response =
                  W.ListWorkflowRunsResponse
                    { W.listWorkflowRunsResponseSuccess = True,
                      W.listWorkflowRunsResponseWorkflowRuns = [workflowRunSummary],
                      W.listWorkflowRunsResponseNextPageToken = Just "next_page_token_test"
                    }
                json = toJSON response
             in case fromJSON json of
                  Success r -> r @?= response
                  Error err -> assertFailure $ "Round trip failed: " ++ err
        ],
      testGroup
        "BatchRunWorkflowRequest"
        [ testCase "Basic request serialization" $
            let requestJson =
                  Aeson.object
                    [ "workflowId" .= ("workflow_test" :: Text),
                      "inputs"
                        .= [ Aeson.object
                               [ "file"
                                   .= Aeson.object
                                     [ "fileName" .= ("test.pdf" :: Text),
                                       "fileUrl" .= ("https://example.com/test.pdf" :: Text)
                                     ]
                               ]
                           ],
                      "version" .= ("1" :: Text)
                    ]
             in case fromJSON requestJson of
                  Success r -> do
                    W.batchRunWorkflowRequestWorkflowId r @?= "workflow_test"
                    length (W.batchRunWorkflowRequestInputs r) @?= 1
                    case W.batchRunWorkflowRequestInputs r of
                      [input] -> do
                        case W.batchWorkflowInputFile input of
                          Just file -> do
                            W.batchInputFileName file @?= Just "test.pdf"
                            W.batchInputFileUrl file @?= Just "https://example.com/test.pdf"
                          Nothing -> assertFailure "Expected file in input"
                      _ -> assertFailure "Expected one input"
                    W.batchRunWorkflowRequestVersion r @?= Just "1"
                  Error err -> assertFailure $ "Failed to parse: " ++ err,
          testCase "Request with raw text input" $
            let requestJson =
                  Aeson.object
                    [ "workflowId" .= ("workflow_test" :: Text),
                      "inputs"
                        .= [ Aeson.object
                               [ "rawText" .= ("Raw text content" :: Text)
                               ]
                           ]
                    ]
             in case fromJSON requestJson of
                  Success r -> do
                    W.batchRunWorkflowRequestWorkflowId r @?= "workflow_test"
                    length (W.batchRunWorkflowRequestInputs r) @?= 1
                    case W.batchRunWorkflowRequestInputs r of
                      [input] -> do
                        W.batchWorkflowInputRawText input @?= Just "Raw text content"
                      _ -> assertFailure "Expected one input"
                    W.batchRunWorkflowRequestVersion r @?= Nothing
                  Error err -> assertFailure $ "Failed to parse: " ++ err
        ],
      testGroup
        "BatchRunWorkflowResponse"
        [ testCase "Basic response deserialization" $
            let json =
                  Aeson.object
                    [ "success" .= True,
                      "batchId" .= ("batch_test_id" :: Text)
                    ]
             in case fromJSON json of
                  Success r -> do
                    W.batchRunWorkflowResponseSuccess r @?= True
                    W.batchRunWorkflowResponseBatchId r @?= "batch_test_id"
                  Error err -> assertFailure $ "Failed to parse: " ++ err,
          testCase "Response serialization/deserialization round trip" $
            let response =
                  W.BatchRunWorkflowResponse
                    { W.batchRunWorkflowResponseSuccess = True,
                      W.batchRunWorkflowResponseBatchId = "batch_test_id"
                    }
                json = toJSON response
             in case fromJSON json of
                  Success r -> r @?= response
                  Error err -> assertFailure $ "Round trip failed: " ++ err
        ]
    ]