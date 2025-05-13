-- | Error types and handling for the Extend API
module Extend.V1.Error
    ( -- * Error types
      ApiError(..)
    , ApiErrorDetail(..)
    , ErrorResponse(..)
    ) where

import Extend.Prelude

-- | Error detail from the Extend API
data ApiErrorDetail = ApiErrorDetail
    { message :: Text
    -- ^ Error message
    , param :: Maybe Text
    -- ^ The parameter that caused the error (if applicable)
    , code :: Maybe Text
    -- ^ Error code
    } deriving stock (Show, Eq, Generic)
      deriving anyclass (ToJSON)

instance FromJSON ApiErrorDetail where
    parseJSON = withObject "ApiErrorDetail" $ \v -> do
        message <- v .: "message"
        param <- v .:? "param"
        code <- v .:? "code"
        pure ApiErrorDetail{..}

-- | API error from the Extend API
data ApiError = ApiError
    { type_ :: Text
    -- ^ Error type
    , message :: Text
    -- ^ Error message
    , details :: Maybe [ApiErrorDetail]
    -- ^ Error details
    , errors :: Maybe (Map Text [Text])
    -- ^ Field-specific errors
    } deriving stock (Show, Eq, Generic)
      deriving anyclass (ToJSON)

instance FromJSON ApiError where
    parseJSON = withObject "ApiError" $ \v -> do
        type_ <- v .: "error"
        message <- v .: "message"
        details <- v .:? "details"
        errors <- v .:? "errors"
        pure ApiError{..}

-- | Error response from the Extend API
data ErrorResponse = ErrorResponse
    { status :: Int
    -- ^ HTTP status code
    , error :: ApiError
    -- ^ Error details
    } deriving stock (Show, Eq, Generic)
      deriving anyclass (FromJSON, ToJSON) 