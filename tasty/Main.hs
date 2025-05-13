{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson (Result (Error, Success), fromJSON, toJSON)
import Data.Text (Text)
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
                  RunWorkflowRequest
                    "wf_test" -- workflowId
                    Nothing -- files
                    Nothing -- rawTexts
                    Nothing -- priority
                    Nothing -- metadata
                json = toJSON request
             in case fromJSON json of
                  Success r ->
                    let RunWorkflowRequest wfId _ _ _ _ = r
                     in wfId @?= "wf_test"
                  Error err -> assertFailure $ "Failed to parse: " ++ err
        ]
    ]