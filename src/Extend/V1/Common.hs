-- | Common types and utilities for the Extend API
module Extend.V1.Common
    ( -- * Common types
      ObjectType(..)
    , SuccessResponse(..)
    , Pagination(..)
    , PaginatedResponse(..)
    ) where

import Extend.Prelude

-- | Object type in the Extend API
data ObjectType
    = WorkflowObject
    | WorkflowRunObject
    | FileObject
    | ProcessorObject
    | ProcessorRunObject
    deriving stock (Show, Eq, Generic)

instance FromJSON ObjectType where
    parseJSON = withText "ObjectType" $ \case
        "workflow" -> pure WorkflowObject
        "workflow_run" -> pure WorkflowRunObject
        "file" -> pure FileObject
        "processor" -> pure ProcessorObject
        "processor_run" -> pure ProcessorRunObject
        _ -> fail "Unknown object type"

instance ToJSON ObjectType where
    toJSON = \case
        WorkflowObject -> String "workflow"
        WorkflowRunObject -> String "workflow_run"
        FileObject -> String "file"
        ProcessorObject -> String "processor"
        ProcessorRunObject -> String "processor_run"

-- | Generic success response
data SuccessResponse a = SuccessResponse
    { success :: Bool
    -- ^ Whether the request was successful
    , data_ :: a
    -- ^ The response data
    } deriving stock (Show, Eq, Generic)

instance (FromJSON a) => FromJSON (SuccessResponse a) where
    parseJSON = withObject "SuccessResponse" $ \v -> do
        success <- v .: "success"
        data_ <- v .: "data"
        pure SuccessResponse{..}

instance (ToJSON a) => ToJSON (SuccessResponse a) where
    toJSON SuccessResponse{..} = object
        [ "success" .= success
        , "data" .= data_
        ]

-- | Pagination information
data Pagination = Pagination
    { nextToken :: Maybe Text
    -- ^ Token for the next page
    , prevToken :: Maybe Text
    -- ^ Token for the previous page
    , total :: Maybe Int
    -- ^ Total number of results
    } deriving stock (Show, Eq, Generic)
      deriving anyclass (FromJSON, ToJSON)

-- | Paginated response
data PaginatedResponse a = PaginatedResponse
    { success :: Bool
    -- ^ Whether the request was successful
    , data_ :: [a]
    -- ^ The response data
    , pagination :: Pagination
    -- ^ Pagination information
    } deriving stock (Show, Eq, Generic)

instance (FromJSON a) => FromJSON (PaginatedResponse a) where
    parseJSON = withObject "PaginatedResponse" $ \v -> do
        success <- v .: "success"
        data_ <- v .: "data"
        pagination <- v .: "pagination"
        pure PaginatedResponse{..}

instance (ToJSON a) => ToJSON (PaginatedResponse a) where
    toJSON PaginatedResponse{..} = object
        [ "success" .= success
        , "data" .= data_
        , "pagination" .= pagination
        ] 