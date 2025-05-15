-- | Common types and utilities for the Extend API
module Extend.V1.Common
  ( -- * Common types
    ObjectType (..),
    SuccessResponse (..),
    Pagination (..),
    PaginatedResponse (..),
  )
where

import Data.Aeson (withText)
import qualified Data.Aeson as Aeson
import Extend.Prelude

-- | Object type in the Extend API
data ObjectType
  = WorkflowObject
  | WorkflowRunObject
  | FileObject
  | ProcessorObject
  | ProcessorRunObject
  | DocumentProcessorObject
  | DocumentProcessorVersionObject
  | DocumentProcessorRunObject
  | WorkflowStepRunObject
  | WorkflowStepObject
  | BatchProcessorRunObject
  deriving stock (Show, Eq, Generic)

instance FromJSON ObjectType where
  parseJSON = withText "ObjectType" $ \case
    "workflow" -> pure WorkflowObject
    "workflow_run" -> pure WorkflowRunObject
    "file" -> pure FileObject
    "processor" -> pure ProcessorObject
    "processor_run" -> pure ProcessorRunObject
    "document_processor" -> pure DocumentProcessorObject
    "document_processor_version" -> pure DocumentProcessorVersionObject
    "document_processor_run" -> pure DocumentProcessorRunObject
    "workflow_step_run" -> pure WorkflowStepRunObject
    "workflow_step" -> pure WorkflowStepObject
    "batch_processor_run" -> pure BatchProcessorRunObject
    _ -> fail "Unknown object type"

instance ToJSON ObjectType where
  toJSON = \case
    WorkflowObject -> String "workflow"
    WorkflowRunObject -> String "workflow_run"
    FileObject -> String "file"
    ProcessorObject -> String "processor"
    ProcessorRunObject -> String "processor_run"
    DocumentProcessorObject -> String "document_processor"
    DocumentProcessorVersionObject -> String "document_processor_version"
    DocumentProcessorRunObject -> String "document_processor_run"
    WorkflowStepRunObject -> String "workflow_step_run"
    WorkflowStepObject -> String "workflow_step"
    BatchProcessorRunObject -> String "batch_processor_run"

-- | Generic success response
data SuccessResponse a = SuccessResponse
  { -- | Whether the request was successful
    success :: Bool,
    -- | The response data
    data_ :: a
  }
  deriving stock (Show, Eq, Generic)

instance (FromJSON a) => FromJSON (SuccessResponse a) where
  parseJSON = Aeson.withObject "SuccessResponse" $ \v -> do
    success <- v Aeson..: "success"
    data_ <- v Aeson..: "data"
    pure SuccessResponse {..}

instance (ToJSON a) => ToJSON (SuccessResponse a) where
  toJSON SuccessResponse {..} =
    Aeson.object
      [ "success" .= success,
        "data" .= data_
      ]

-- | Pagination information
data Pagination = Pagination
  { -- | Token for the next page
    nextToken :: Maybe Text,
    -- | Token for the previous page
    prevToken :: Maybe Text,
    -- | Total number of results
    total :: Maybe Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Paginated response
data PaginatedResponse a = PaginatedResponse
  { -- | Whether the request was successful
    success :: Bool,
    -- | The response data
    data_ :: [a],
    -- | Pagination information
    pagination :: Pagination
  }
  deriving stock (Show, Eq, Generic)

instance (FromJSON a) => FromJSON (PaginatedResponse a) where
  parseJSON = Aeson.withObject "PaginatedResponse" $ \v -> do
    success <- v Aeson..: "success"
    data_ <- v Aeson..: "data"
    pagination <- v Aeson..: "pagination"
    pure PaginatedResponse {..}

instance (ToJSON a) => ToJSON (PaginatedResponse a) where
  toJSON PaginatedResponse {..} =
    Aeson.object
      [ "success" .= success,
        "data" .= data_,
        "pagination" .= pagination
      ]