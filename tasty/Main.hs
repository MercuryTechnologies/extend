{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson (Result (Error, Success), ToJSON, Value, fromJSON, toJSON, (.=))
import qualified Data.Aeson as Aeson (object)
import Data.Text (Text)
import Data.Time.Calendar (Day (..))
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)
import Extend.V1
import qualified Extend.V1.Processors as P
import qualified Extend.V1.Workflows as W
import Test.Tasty
import Test.Tasty.HUnit

instance ToJSON P.RunProcessorResponse where
  toJSON r =
    Aeson.object
      [ "success" .= P.runProcessorResponseSuccess r,
        "processorRun" .= P.runProcessorResponseProcessorRun r
      ]

instance ToJSON P.GetProcessorRunResponse where
  toJSON r =
    Aeson.object
      [ "success" .= P.getProcessorRunResponseSuccess r,
        "processorRun" .= P.getProcessorRunResponseProcessorRun r
      ]

instance ToJSON P.CreateProcessorResponse where
  toJSON r =
    Aeson.object
      [ "success" .= P.createProcessorResponseSuccess r,
        "processor" .= P.createProcessorResponseProcessor r
      ]

instance ToJSON P.UpdateProcessorResponse where
  toJSON r =
    Aeson.object
      [ "success" .= P.updateProcessorResponseSuccess r,
        "processor" .= P.updateProcessorResponseProcessor r
      ]

instance ToJSON P.GetProcessorVersionResponse where
  toJSON r =
    Aeson.object
      [ "success" .= P.getProcessorVersionResponseSuccess r,
        "version" .= P.getProcessorVersionResponseVersion r
      ]

instance ToJSON P.ListProcessorVersionsResponse where
  toJSON r =
    Aeson.object
      [ "success" .= P.listProcessorVersionsResponseSuccess r,
        "versions" .= P.listProcessorVersionsResponseVersions r
      ]

instance ToJSON P.PublishProcessorVersionResponse where
  toJSON r =
    Aeson.object
      [ "success" .= P.publishProcessorVersionResponseSuccess r,
        "processorVersion" .= P.publishProcessorVersionResponseProcessorVersion r
      ]

instance ToJSON P.GetBatchProcessorRunResponse where
  toJSON r =
    Aeson.object
      [ "success" .= P.getBatchProcessorRunResponseSuccess r,
        "batchProcessorRun" .= P.getBatchProcessorRunResponseBatchProcessorRun r
      ]

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
        "ProcessorType"
        [ testCase "Extract JSON round trip" $
            let processorType = P.Extract
                json = toJSON processorType
                parsed = fromJSON json
             in parsed @?= Success P.Extract,
          testCase "Classify JSON round trip" $
            let processorType = P.Classify
                json = toJSON processorType
                parsed = fromJSON json
             in parsed @?= Success P.Classify,
          testCase "Splitter JSON round trip" $
            let processorType = P.Splitter
                json = toJSON processorType
                parsed = fromJSON json
             in parsed @?= Success P.Splitter
        ],
      testGroup
        "ProcessorRunStatus"
        [ testCase "Processing JSON round trip" $
            let status = P.Processing
                json = toJSON status
                parsed = fromJSON json
             in parsed @?= Success P.Processing,
          testCase "Processed JSON round trip" $
            let status = P.Processed
                json = toJSON status
                parsed = fromJSON json
             in parsed @?= Success P.Processed,
          testCase "Failed JSON round trip" $
            let status = P.Failed
                json = toJSON status
                parsed = fromJSON json
             in parsed @?= Success P.Failed
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
        ],
      testGroup
        "CreateWorkflowRequest"
        [ testCase "Basic request serialization" $
            let request =
                  W.CreateWorkflowRequest
                    "Test Workflow" -- name
                json = toJSON request
             in case fromJSON json of
                  Success r ->
                    let W.CreateWorkflowRequest name = r
                     in name @?= "Test Workflow"
                  Error err -> assertFailure $ "Failed to parse: " ++ err
        ],
      testGroup
        "CreateWorkflowResponse"
        [ testCase "Basic response deserialization" $
            let json =
                  Aeson.object
                    [ "success" .= True,
                      "workflow"
                        .= Aeson.object
                          [ "object" .= ("workflow" :: Text),
                            "id" .= ("workflow_test123" :: Text),
                            "version" .= ("draft" :: Text),
                            "name" .= ("Test Workflow" :: Text)
                          ]
                    ]
             in case fromJSON json of
                  Success r -> do
                    W.createWorkflowResponseSuccess r @?= True
                    let workflow = W.createWorkflowResponseWorkflow r
                    W.createdWorkflowObjectId workflow @?= "workflow_test123"
                    W.createdWorkflowObjectVersion workflow @?= "draft"
                    W.createdWorkflowObjectName workflow @?= "Test Workflow"
                    W.createdWorkflowObjectType workflow @?= WorkflowObject
                  Error err -> assertFailure $ "Failed to parse: " ++ err,
          testCase "Response serialization/deserialization round trip" $
            let workflowObject =
                  W.CreatedWorkflowObject
                    { W.createdWorkflowObjectType = WorkflowObject,
                      W.createdWorkflowObjectId = "workflow_test123",
                      W.createdWorkflowObjectVersion = "draft",
                      W.createdWorkflowObjectName = "Test Workflow"
                    }
                response =
                  W.CreateWorkflowResponse
                    { W.createWorkflowResponseSuccess = True,
                      W.createWorkflowResponseWorkflow = workflowObject
                    }
                json = toJSON response
             in case fromJSON json of
                  Success r -> r @?= response
                  Error err -> assertFailure $ "Round trip failed: " ++ err
        ],
      -- Processor API tests
      testGroup
        "RunProcessorRequest"
        [ testCase "Basic request serialization" $
            let fileInput =
                  P.ProcessorRunFileInput
                    { P.processorRunFileInputFileName = Just "test.pdf",
                      P.processorRunFileInputFileUrl = Just "https://example.com/test.pdf",
                      P.processorRunFileInputFileId = Nothing
                    }
                request =
                  P.RunProcessorRequest
                    { P.runProcessorRequestProcessorId = "dp_test123",
                      P.runProcessorRequestVersion = Just "1.0",
                      P.runProcessorRequestFile = Just fileInput,
                      P.runProcessorRequestRawText = Nothing,
                      P.runProcessorRequestPriority = Just 10,
                      P.runProcessorRequestMetadata = Nothing,
                      P.runProcessorRequestConfig = Nothing
                    }
                json = toJSON request
             in case fromJSON json of
                  Success r -> do
                    P.runProcessorRequestProcessorId r @?= "dp_test123"
                    P.runProcessorRequestVersion r @?= Just "1.0"
                    case P.runProcessorRequestFile r of
                      Just file -> do
                        P.processorRunFileInputFileName file @?= Just "test.pdf"
                        P.processorRunFileInputFileUrl file @?= Just "https://example.com/test.pdf"
                      Nothing -> assertFailure "Expected file input"
                    P.runProcessorRequestPriority r @?= Just 10
                  Error err -> assertFailure $ "Failed to parse: " ++ err,
          testCase "Request with raw text serialization" $
            let request =
                  P.RunProcessorRequest
                    { P.runProcessorRequestProcessorId = "dp_test123",
                      P.runProcessorRequestVersion = Nothing,
                      P.runProcessorRequestFile = Nothing,
                      P.runProcessorRequestRawText = Just "Raw text content",
                      P.runProcessorRequestPriority = Nothing,
                      P.runProcessorRequestMetadata = Nothing,
                      P.runProcessorRequestConfig = Nothing
                    }
                json = toJSON request
             in case fromJSON json of
                  Success r -> do
                    P.runProcessorRequestProcessorId r @?= "dp_test123"
                    P.runProcessorRequestRawText r @?= Just "Raw text content"
                  Error err -> assertFailure $ "Failed to parse: " ++ err
        ],
      testGroup
        "RunProcessorResponse"
        [ testCase "Basic response deserialization" $
            let json =
                  Aeson.object
                    [ "success" .= True,
                      "processorRun"
                        .= Aeson.object
                          [ "object" .= ("document_processor_run" :: Text),
                            "id" .= ("dpr_test123" :: Text),
                            "processorId" .= ("dp_test123" :: Text),
                            "processorVersionId" .= ("dpv_test123" :: Text),
                            "processorName" .= ("Test Processor" :: Text),
                            "status" .= ("PROCESSING" :: Text),
                            "output" .= Aeson.object [],
                            "reviewed" .= False,
                            "edited" .= False,
                            "edits" .= Aeson.object [],
                            "type" .= ("EXTRACT" :: Text),
                            "config" .= Aeson.object ["type" .= ("EXTRACT" :: Text)],
                            "files" .= ([] :: [Value]),
                            "mergedProcessors" .= ([] :: [Value]),
                            "url" .= ("https://example.com/run" :: Text)
                          ]
                    ]
             in case fromJSON json of
                  Success r -> do
                    P.runProcessorResponseSuccess r @?= True
                    let processorRun = P.runProcessorResponseProcessorRun r
                    P.processorRunId processorRun @?= "dpr_test123"
                    P.processorRunProcessorId processorRun @?= "dp_test123"
                    P.processorRunProcessorVersionId processorRun @?= "dpv_test123"
                    P.processorRunProcessorName processorRun @?= "Test Processor"
                    P.processorRunStatus processorRun @?= P.Processing
                    P.processorRunReviewed processorRun @?= False
                    P.processorRunEdited processorRun @?= False
                    P.processorRunType processorRun @?= Just "EXTRACT"
                    P.processorRunUrl processorRun @?= Just "https://example.com/run"
                  Error err -> assertFailure $ "Failed to parse: " ++ err,
          testCase "Response serialization/deserialization round trip" $
            let processorRun =
                  P.ProcessorRun
                    { P.processorRunObject = DocumentProcessorRunObject,
                      P.processorRunId = "dpr_test123",
                      P.processorRunProcessorId = "dp_test123",
                      P.processorRunProcessorVersionId = "dpv_test123",
                      P.processorRunProcessorName = "Test Processor",
                      P.processorRunStatus = P.Processing,
                      P.processorRunOutput = toJSON ([] :: [Int]),
                      P.processorRunFailureReason = Nothing,
                      P.processorRunFailureMessage = Nothing,
                      P.processorRunMetadata = Nothing,
                      P.processorRunReviewed = False,
                      P.processorRunEdited = False,
                      P.processorRunEdits = toJSON ([] :: [Int]),
                      P.processorRunType = Just "EXTRACT",
                      P.processorRunConfig = P.ProcessorConfig (toJSON ([] :: [Int])),
                      P.processorRunInitialOutput = Nothing,
                      P.processorRunReviewedOutput = Nothing,
                      P.processorRunFiles = [],
                      P.processorRunMergedProcessors = [],
                      P.processorRunUrl = Just "https://example.com/run"
                    }
                response =
                  P.RunProcessorResponse
                    { P.runProcessorResponseSuccess = True,
                      P.runProcessorResponseProcessorRun = processorRun
                    }
                json = toJSON response
             in case fromJSON json of
                  Success r -> r @?= response
                  Error err -> assertFailure $ "Round trip failed: " ++ err
        ],
      testGroup
        "GetProcessorRunResponse"
        [ testCase "Basic response deserialization" $
            let json =
                  Aeson.object
                    [ "success" .= True,
                      "processorRun"
                        .= Aeson.object
                          [ "object" .= ("document_processor_run" :: Text),
                            "id" .= ("dpr_test123" :: Text),
                            "processorId" .= ("dp_test123" :: Text),
                            "processorVersionId" .= ("dpv_test123" :: Text),
                            "processorName" .= ("Test Processor" :: Text),
                            "status" .= ("PROCESSED" :: Text),
                            "output" .= Aeson.object [],
                            "reviewed" .= True,
                            "edited" .= True,
                            "edits" .= Aeson.object [],
                            "type" .= ("EXTRACT" :: Text),
                            "config" .= Aeson.object ["type" .= ("EXTRACT" :: Text)],
                            "files" .= ([] :: [Value]),
                            "mergedProcessors" .= ([] :: [Value]),
                            "url" .= ("https://example.com/run" :: Text)
                          ]
                    ]
             in case fromJSON json of
                  Success r -> do
                    P.getProcessorRunResponseSuccess r @?= True
                    let processorRun = P.getProcessorRunResponseProcessorRun r
                    P.processorRunId processorRun @?= "dpr_test123"
                    P.processorRunProcessorId processorRun @?= "dp_test123"
                    P.processorRunStatus processorRun @?= P.Processed
                    P.processorRunReviewed processorRun @?= True
                    P.processorRunEdited processorRun @?= True
                  Error err -> assertFailure $ "Failed to parse: " ++ err,
          testCase "Response serialization/deserialization round trip" $
            let processorRun =
                  P.ProcessorRun
                    { P.processorRunObject = DocumentProcessorRunObject,
                      P.processorRunId = "dpr_test123",
                      P.processorRunProcessorId = "dp_test123",
                      P.processorRunProcessorVersionId = "dpv_test123",
                      P.processorRunProcessorName = "Test Processor",
                      P.processorRunStatus = P.Processed,
                      P.processorRunOutput = toJSON ([] :: [Int]),
                      P.processorRunFailureReason = Nothing,
                      P.processorRunFailureMessage = Nothing,
                      P.processorRunMetadata = Nothing,
                      P.processorRunReviewed = True,
                      P.processorRunEdited = True,
                      P.processorRunEdits = toJSON ([] :: [Int]),
                      P.processorRunType = Just "EXTRACT",
                      P.processorRunConfig = P.ProcessorConfig (toJSON ([] :: [Int])),
                      P.processorRunInitialOutput = Nothing,
                      P.processorRunReviewedOutput = Nothing,
                      P.processorRunFiles = [],
                      P.processorRunMergedProcessors = [],
                      P.processorRunUrl = Just "https://example.com/run"
                    }
                response =
                  P.GetProcessorRunResponse
                    { P.getProcessorRunResponseSuccess = True,
                      P.getProcessorRunResponseProcessorRun = processorRun
                    }
                json = toJSON response
             in case fromJSON json of
                  Success r -> r @?= response
                  Error err -> assertFailure $ "Round trip failed: " ++ err
        ],
      testGroup
        "CreateProcessorRequest"
        [ testCase "Basic request serialization" $
            let request =
                  P.CreateProcessorRequest
                    { P.createProcessorRequestName = "Test Processor",
                      P.createProcessorRequestType = P.Extract,
                      P.createProcessorRequestCloneProcessorId = Nothing,
                      P.createProcessorRequestConfig = Nothing
                    }
                json = toJSON request
             in case fromJSON json of
                  Success r -> do
                    P.createProcessorRequestName r @?= "Test Processor"
                    P.createProcessorRequestType r @?= P.Extract
                    P.createProcessorRequestCloneProcessorId r @?= Nothing
                  Error err -> assertFailure $ "Failed to parse: " ++ err,
          testCase "Request with cloneProcessorId serialization" $
            let request =
                  P.CreateProcessorRequest
                    { P.createProcessorRequestName = "Test Processor Clone",
                      P.createProcessorRequestType = P.Classify,
                      P.createProcessorRequestCloneProcessorId = Just "dp_source123",
                      P.createProcessorRequestConfig = Nothing
                    }
                json = toJSON request
             in case fromJSON json of
                  Success r -> do
                    P.createProcessorRequestName r @?= "Test Processor Clone"
                    P.createProcessorRequestType r @?= P.Classify
                    P.createProcessorRequestCloneProcessorId r @?= Just "dp_source123"
                  Error err -> assertFailure $ "Failed to parse: " ++ err
        ],
      testGroup
        "CreateProcessorResponse"
        [ testCase "Basic response deserialization" $
            let json =
                  Aeson.object
                    [ "success" .= True,
                      "processor"
                        .= Aeson.object
                          [ "object" .= ("document_processor" :: Text),
                            "id" .= ("dp_test123" :: Text),
                            "name" .= ("Test Processor" :: Text),
                            "type" .= ("EXTRACT" :: Text),
                            "createdAt" .= ("2025-04-28T17:01:39.285Z" :: Text),
                            "updatedAt" .= ("2025-04-28T17:01:39.285Z" :: Text),
                            "draftVersion"
                              .= Aeson.object
                                [ "object" .= ("document_processor_version" :: Text),
                                  "id" .= ("dpv_test123" :: Text),
                                  "processorId" .= ("dp_test123" :: Text),
                                  "processorType" .= ("EXTRACT" :: Text),
                                  "version" .= ("draft" :: Text),
                                  "config" .= Aeson.object ["type" .= ("EXTRACT" :: Text)],
                                  "createdAt" .= ("2025-04-28T17:01:39.285Z" :: Text),
                                  "updatedAt" .= ("2025-04-28T17:01:39.285Z" :: Text)
                                ]
                          ]
                    ]
             in case fromJSON json of
                  Success r -> do
                    P.createProcessorResponseSuccess r @?= True
                    let processor = P.createProcessorResponseProcessor r
                    P.processorId processor @?= "dp_test123"
                    P.processorName processor @?= "Test Processor"
                    P.processorType processor @?= P.Extract
                    case P.processorDraftVersion processor of
                      Just draftVersion -> do
                        P.processorVersionId draftVersion @?= "dpv_test123"
                        P.processorVersionVersion draftVersion @?= "draft"
                      Nothing -> assertFailure "Expected draft version"
                  Error err -> assertFailure $ "Failed to parse: " ++ err,
          testCase "Response serialization/deserialization round trip" $
            let testTime = UTCTime (ModifiedJulianDay 59000) (secondsToDiffTime 0)
                processorVersion =
                  P.ProcessorVersion
                    { P.processorVersionObject = DocumentProcessorVersionObject,
                      P.processorVersionId = "dpv_test123",
                      P.processorVersionProcessorId = "dp_test123",
                      P.processorVersionProcessorName = Just "Test Processor",
                      P.processorVersionProcessorType = P.Extract,
                      P.processorVersionDescription = Nothing,
                      P.processorVersionVersion = "draft",
                      P.processorVersionConfig = P.ProcessorConfig (toJSON ([] :: [Int])),
                      P.processorVersionCreatedAt = testTime,
                      P.processorVersionUpdatedAt = testTime
                    }
                processor =
                  P.Processor
                    { P.processorObject = DocumentProcessorObject,
                      P.processorId = "dp_test123",
                      P.processorName = "Test Processor",
                      P.processorType = P.Extract,
                      P.processorCreatedAt = testTime,
                      P.processorUpdatedAt = testTime,
                      P.processorDraftVersion = Just processorVersion
                    }
                response =
                  P.CreateProcessorResponse
                    { P.createProcessorResponseSuccess = True,
                      P.createProcessorResponseProcessor = processor
                    }
                json = toJSON response
             in case fromJSON json of
                  Success r -> r @?= response
                  Error err -> assertFailure $ "Round trip failed: " ++ err
        ],
      testGroup
        "UpdateProcessorRequest"
        [ testCase "Basic request serialization" $
            let request =
                  P.UpdateProcessorRequest
                    { P.updateProcessorRequestName = Just "Updated Processor Name",
                      P.updateProcessorRequestConfig = Nothing
                    }
                json = toJSON request
             in case fromJSON json of
                  Success r -> do
                    P.updateProcessorRequestName r @?= Just "Updated Processor Name"
                    P.updateProcessorRequestConfig r @?= Nothing
                  Error err -> assertFailure $ "Failed to parse: " ++ err,
          testCase "Request with config serialization" $
            let config = P.ProcessorConfig (toJSON ([] :: [Int]))
                request =
                  P.UpdateProcessorRequest
                    { P.updateProcessorRequestName = Nothing,
                      P.updateProcessorRequestConfig = Just config
                    }
                json = toJSON request
             in case fromJSON json of
                  Success r -> do
                    P.updateProcessorRequestName r @?= Nothing
                    P.updateProcessorRequestConfig r @?= Just config
                  Error err -> assertFailure $ "Failed to parse: " ++ err
        ],
      testGroup
        "UpdateProcessorResponse"
        [ testCase "Basic response deserialization" $
            let json =
                  Aeson.object
                    [ "success" .= True,
                      "processor"
                        .= Aeson.object
                          [ "object" .= ("document_processor" :: Text),
                            "id" .= ("dp_test123" :: Text),
                            "name" .= ("Updated Processor Name" :: Text),
                            "type" .= ("EXTRACT" :: Text),
                            "createdAt" .= ("2025-04-28T17:01:39.285Z" :: Text),
                            "updatedAt" .= ("2025-04-28T17:01:39.285Z" :: Text),
                            "draftVersion"
                              .= Aeson.object
                                [ "object" .= ("document_processor_version" :: Text),
                                  "id" .= ("dpv_test123" :: Text),
                                  "processorId" .= ("dp_test123" :: Text),
                                  "processorType" .= ("EXTRACT" :: Text),
                                  "version" .= ("draft" :: Text),
                                  "config" .= Aeson.object ["type" .= ("EXTRACT" :: Text)],
                                  "createdAt" .= ("2025-04-28T17:01:39.285Z" :: Text),
                                  "updatedAt" .= ("2025-04-28T17:01:39.285Z" :: Text)
                                ]
                          ]
                    ]
             in case fromJSON json of
                  Success r -> do
                    P.updateProcessorResponseSuccess r @?= True
                    let processor = P.updateProcessorResponseProcessor r
                    P.processorName processor @?= "Updated Processor Name"
                  Error err -> assertFailure $ "Failed to parse: " ++ err,
          testCase "Response serialization/deserialization round trip" $
            let testTime = UTCTime (ModifiedJulianDay 59000) (secondsToDiffTime 0)
                processorVersion =
                  P.ProcessorVersion
                    { P.processorVersionObject = DocumentProcessorVersionObject,
                      P.processorVersionId = "dpv_test123",
                      P.processorVersionProcessorId = "dp_test123",
                      P.processorVersionProcessorName = Just "Updated Processor Name",
                      P.processorVersionProcessorType = P.Extract,
                      P.processorVersionDescription = Nothing,
                      P.processorVersionVersion = "draft",
                      P.processorVersionConfig = P.ProcessorConfig (toJSON ([] :: [Int])),
                      P.processorVersionCreatedAt = testTime,
                      P.processorVersionUpdatedAt = testTime
                    }
                processor =
                  P.Processor
                    { P.processorObject = DocumentProcessorObject,
                      P.processorId = "dp_test123",
                      P.processorName = "Updated Processor Name",
                      P.processorType = P.Extract,
                      P.processorCreatedAt = testTime,
                      P.processorUpdatedAt = testTime,
                      P.processorDraftVersion = Just processorVersion
                    }
                response =
                  P.UpdateProcessorResponse
                    { P.updateProcessorResponseSuccess = True,
                      P.updateProcessorResponseProcessor = processor
                    }
                json = toJSON response
             in case fromJSON json of
                  Success r -> r @?= response
                  Error err -> assertFailure $ "Round trip failed: " ++ err
        ],
      testGroup
        "GetProcessorVersionResponse"
        [ testCase "Basic response deserialization" $
            let json =
                  Aeson.object
                    [ "success" .= True,
                      "version"
                        .= Aeson.object
                          [ "object" .= ("document_processor_version" :: Text),
                            "id" .= ("dpv_test123" :: Text),
                            "processorId" .= ("dp_test123" :: Text),
                            "processorType" .= ("EXTRACT" :: Text),
                            "version" .= ("1.0" :: Text),
                            "config" .= Aeson.object ["type" .= ("EXTRACT" :: Text)],
                            "createdAt" .= ("2025-04-28T17:01:39.285Z" :: Text),
                            "updatedAt" .= ("2025-04-28T17:01:39.285Z" :: Text)
                          ]
                    ]
             in case fromJSON json of
                  Success r -> do
                    P.getProcessorVersionResponseSuccess r @?= True
                    let version = P.getProcessorVersionResponseVersion r
                    P.processorVersionId version @?= "dpv_test123"
                    P.processorVersionVersion version @?= "1.0"
                  Error err -> assertFailure $ "Failed to parse: " ++ err,
          testCase "Response serialization/deserialization round trip" $
            let testTime = UTCTime (ModifiedJulianDay 59000) (secondsToDiffTime 0)
                processorVersion =
                  P.ProcessorVersion
                    { P.processorVersionObject = DocumentProcessorVersionObject,
                      P.processorVersionId = "dpv_test123",
                      P.processorVersionProcessorId = "dp_test123",
                      P.processorVersionProcessorName = Just "Test Processor",
                      P.processorVersionProcessorType = P.Extract,
                      P.processorVersionDescription = Just "Version description",
                      P.processorVersionVersion = "1.0",
                      P.processorVersionConfig = P.ProcessorConfig (toJSON ([] :: [Int])),
                      P.processorVersionCreatedAt = testTime,
                      P.processorVersionUpdatedAt = testTime
                    }
                response =
                  P.GetProcessorVersionResponse
                    { P.getProcessorVersionResponseSuccess = True,
                      P.getProcessorVersionResponseVersion = processorVersion
                    }
                json = toJSON response
             in case fromJSON json of
                  Success r -> r @?= response
                  Error err -> assertFailure $ "Round trip failed: " ++ err
        ],
      testGroup
        "ListProcessorVersionsResponse"
        [ testCase "Basic response deserialization" $
            let json =
                  Aeson.object
                    [ "success" .= True,
                      "versions"
                        .= [ Aeson.object
                               [ "object" .= ("document_processor_version" :: Text),
                                 "id" .= ("dpv_test123" :: Text),
                                 "processorId" .= ("dp_test123" :: Text),
                                 "processorType" .= ("EXTRACT" :: Text),
                                 "version" .= ("1.0" :: Text),
                                 "config" .= Aeson.object ["type" .= ("EXTRACT" :: Text)],
                                 "createdAt" .= ("2025-04-28T17:01:39.285Z" :: Text),
                                 "updatedAt" .= ("2025-04-28T17:01:39.285Z" :: Text)
                               ],
                             Aeson.object
                               [ "object" .= ("document_processor_version" :: Text),
                                 "id" .= ("dpv_draft" :: Text),
                                 "processorId" .= ("dp_test123" :: Text),
                                 "processorType" .= ("EXTRACT" :: Text),
                                 "version" .= ("draft" :: Text),
                                 "config" .= Aeson.object ["type" .= ("EXTRACT" :: Text)],
                                 "createdAt" .= ("2025-04-28T17:01:39.285Z" :: Text),
                                 "updatedAt" .= ("2025-04-28T17:01:39.285Z" :: Text)
                               ]
                           ]
                    ]
             in case fromJSON json of
                  Success r -> do
                    P.listProcessorVersionsResponseSuccess r @?= True
                    let versions = P.listProcessorVersionsResponseVersions r
                    length versions @?= 2
                    case versions of
                      (v1 : v2 : _) -> do
                        P.processorVersionId v1 @?= "dpv_test123"
                        P.processorVersionVersion v1 @?= "1.0"
                        P.processorVersionId v2 @?= "dpv_draft"
                        P.processorVersionVersion v2 @?= "draft"
                      _ -> assertFailure "Expected two versions"
                  Error err -> assertFailure $ "Failed to parse: " ++ err,
          testCase "Response serialization/deserialization round trip" $
            let testTime = UTCTime (ModifiedJulianDay 59000) (secondsToDiffTime 0)
                processorVersion1 =
                  P.ProcessorVersion
                    { P.processorVersionObject = DocumentProcessorVersionObject,
                      P.processorVersionId = "dpv_test123",
                      P.processorVersionProcessorId = "dp_test123",
                      P.processorVersionProcessorName = Just "Test Processor",
                      P.processorVersionProcessorType = P.Extract,
                      P.processorVersionDescription = Just "Version description",
                      P.processorVersionVersion = "1.0",
                      P.processorVersionConfig = P.ProcessorConfig (toJSON ([] :: [Int])),
                      P.processorVersionCreatedAt = testTime,
                      P.processorVersionUpdatedAt = testTime
                    }
                processorVersion2 =
                  P.ProcessorVersion
                    { P.processorVersionObject = DocumentProcessorVersionObject,
                      P.processorVersionId = "dpv_draft",
                      P.processorVersionProcessorId = "dp_test123",
                      P.processorVersionProcessorName = Just "Test Processor",
                      P.processorVersionProcessorType = P.Extract,
                      P.processorVersionDescription = Nothing,
                      P.processorVersionVersion = "draft",
                      P.processorVersionConfig = P.ProcessorConfig (toJSON ([] :: [Int])),
                      P.processorVersionCreatedAt = testTime,
                      P.processorVersionUpdatedAt = testTime
                    }
                response =
                  P.ListProcessorVersionsResponse
                    { P.listProcessorVersionsResponseSuccess = True,
                      P.listProcessorVersionsResponseVersions = [processorVersion1, processorVersion2]
                    }
                json = toJSON response
             in case fromJSON json of
                  Success r -> r @?= response
                  Error err -> assertFailure $ "Round trip failed: " ++ err
        ],
      testGroup
        "PublishProcessorVersionRequest"
        [ testCase "Basic request serialization" $
            let request =
                  P.PublishProcessorVersionRequest
                    { P.publishProcessorVersionRequestReleaseType = "major",
                      P.publishProcessorVersionRequestDescription = Just "Version 2.0 release",
                      P.publishProcessorVersionRequestConfig = Nothing
                    }
                json = toJSON request
             in case fromJSON json of
                  Success r -> do
                    P.publishProcessorVersionRequestReleaseType r @?= "major"
                    P.publishProcessorVersionRequestDescription r @?= Just "Version 2.0 release"
                  Error err -> assertFailure $ "Failed to parse: " ++ err,
          testCase "Request with config serialization" $
            let config = P.ProcessorConfig (toJSON ([] :: [Int]))
                request =
                  P.PublishProcessorVersionRequest
                    { P.publishProcessorVersionRequestReleaseType = "minor",
                      P.publishProcessorVersionRequestDescription = Nothing,
                      P.publishProcessorVersionRequestConfig = Just config
                    }
                json = toJSON request
             in case fromJSON json of
                  Success r -> do
                    P.publishProcessorVersionRequestReleaseType r @?= "minor"
                    P.publishProcessorVersionRequestConfig r @?= Just config
                  Error err -> assertFailure $ "Failed to parse: " ++ err
        ],
      testGroup
        "PublishProcessorVersionResponse"
        [ testCase "Basic response deserialization" $
            let json =
                  Aeson.object
                    [ "success" .= True,
                      "processorVersion"
                        .= Aeson.object
                          [ "object" .= ("document_processor_version" :: Text),
                            "id" .= ("dpv_published" :: Text),
                            "processorId" .= ("dp_test123" :: Text),
                            "processorType" .= ("EXTRACT" :: Text),
                            "description" .= ("Version 2.0 release" :: Text),
                            "version" .= ("2.0" :: Text),
                            "config" .= Aeson.object ["type" .= ("EXTRACT" :: Text)],
                            "createdAt" .= ("2025-04-28T17:01:39.285Z" :: Text),
                            "updatedAt" .= ("2025-04-28T17:01:39.285Z" :: Text)
                          ]
                    ]
             in case fromJSON json of
                  Success r -> do
                    P.publishProcessorVersionResponseSuccess r @?= True
                    let version = P.publishProcessorVersionResponseProcessorVersion r
                    P.processorVersionId version @?= "dpv_published"
                    P.processorVersionVersion version @?= "2.0"
                    P.processorVersionDescription version @?= Just "Version 2.0 release"
                  Error err -> assertFailure $ "Failed to parse: " ++ err,
          testCase "Response serialization/deserialization round trip" $
            let testTime = UTCTime (ModifiedJulianDay 59000) (secondsToDiffTime 0)
                processorVersion =
                  P.ProcessorVersion
                    { P.processorVersionObject = DocumentProcessorVersionObject,
                      P.processorVersionId = "dpv_published",
                      P.processorVersionProcessorId = "dp_test123",
                      P.processorVersionProcessorName = Just "Test Processor",
                      P.processorVersionProcessorType = P.Extract,
                      P.processorVersionDescription = Just "Version 2.0 release",
                      P.processorVersionVersion = "2.0",
                      P.processorVersionConfig = P.ProcessorConfig (toJSON ([] :: [Int])),
                      P.processorVersionCreatedAt = testTime,
                      P.processorVersionUpdatedAt = testTime
                    }
                response =
                  P.PublishProcessorVersionResponse
                    { P.publishProcessorVersionResponseSuccess = True,
                      P.publishProcessorVersionResponseProcessorVersion = processorVersion
                    }
                json = toJSON response
             in case fromJSON json of
                  Success r -> r @?= response
                  Error err -> assertFailure $ "Round trip failed: " ++ err
        ],
      testGroup
        "GetBatchProcessorRunResponse"
        [ testCase "Basic response deserialization" $
            let json =
                  Aeson.object
                    [ "success" .= True,
                      "batchProcessorRun"
                        .= Aeson.object
                          [ "object" .= ("batch_processor_run" :: Text),
                            "id" .= ("bpr_test123" :: Text),
                            "processorId" .= ("dp_test123" :: Text),
                            "processorVersionId" .= ("dpv_test123" :: Text),
                            "processorName" .= ("Test Processor" :: Text),
                            "metrics" .= Aeson.object ["numFiles" .= (10 :: Int)],
                            "status" .= ("PROCESSED" :: Text),
                            "source" .= ("EVAL_SET" :: Text),
                            "sourceId" .= ("ev_test123" :: Text),
                            "runCount" .= (10 :: Int),
                            "options" .= Aeson.object [],
                            "createdAt" .= ("2025-04-28T17:01:39.285Z" :: Text),
                            "updatedAt" .= ("2025-04-28T17:01:39.285Z" :: Text)
                          ]
                    ]
             in case fromJSON json of
                  Success r -> do
                    P.getBatchProcessorRunResponseSuccess r @?= True
                    let batchRun = P.getBatchProcessorRunResponseBatchProcessorRun r
                    P.batchProcessorRunId batchRun @?= "bpr_test123"
                    P.batchProcessorRunProcessorId batchRun @?= "dp_test123"
                    P.batchProcessorRunStatus batchRun @?= "PROCESSED"
                    P.batchProcessorRunSource batchRun @?= "EVAL_SET"
                    P.batchProcessorRunSourceId batchRun @?= Just "ev_test123"
                    P.batchProcessorRunRunCount batchRun @?= 10
                  Error err -> assertFailure $ "Failed to parse: " ++ err,
          testCase "Response serialization/deserialization round trip" $
            let testTime = UTCTime (ModifiedJulianDay 59000) (secondsToDiffTime 0)
                batchProcessorRun =
                  P.BatchProcessorRun
                    { P.batchProcessorRunObject = BatchProcessorRunObject,
                      P.batchProcessorRunId = "bpr_test123",
                      P.batchProcessorRunProcessorId = "dp_test123",
                      P.batchProcessorRunProcessorVersionId = "dpv_test123",
                      P.batchProcessorRunProcessorName = "Test Processor",
                      P.batchProcessorRunMetrics = toJSON ([] :: [Int]),
                      P.batchProcessorRunStatus = "PROCESSED",
                      P.batchProcessorRunSource = "EVAL_SET",
                      P.batchProcessorRunSourceId = Just "ev_test123",
                      P.batchProcessorRunRunCount = 10,
                      P.batchProcessorRunOptions = toJSON ([] :: [Int]),
                      P.batchProcessorRunCreatedAt = testTime,
                      P.batchProcessorRunUpdatedAt = testTime
                    }
                response =
                  P.GetBatchProcessorRunResponse
                    { P.getBatchProcessorRunResponseSuccess = True,
                      P.getBatchProcessorRunResponseBatchProcessorRun = batchProcessorRun
                    }
                json = toJSON response
             in case fromJSON json of
                  Success r -> r @?= response
                  Error err -> assertFailure $ "Round trip failed: " ++ err
        ]
    ]