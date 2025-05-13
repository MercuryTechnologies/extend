-- | Error types and handling for the Extend API
module Extend.V1.Error
  ( -- * Error types
    ApiError (..),
    ApiErrorDetail (..),
    ErrorResponse (..),
  )
where

import qualified Data.Aeson as Aeson
import Extend.Prelude

-- | Error detail from the Extend API
data ApiErrorDetail = ApiErrorDetail
  { -- | Error message
    message :: Text,
    -- | The parameter that caused the error (if applicable)
    param :: Maybe Text,
    -- | Error code
    code :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

instance FromJSON ApiErrorDetail where
  parseJSON = Aeson.withObject "ApiErrorDetail" $ \v -> do
    message <- v Aeson..: "message"
    param <- v Aeson..:? "param"
    code <- v Aeson..:? "code"
    pure ApiErrorDetail {..}

-- | API error from the Extend API
data ApiError = ApiError
  { -- | Error type
    type_ :: Text,
    -- | Error message
    message :: Text,
    -- | Error details
    details :: Maybe [ApiErrorDetail],
    -- | Field-specific errors
    errors :: Maybe (Map Text [Text])
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

instance FromJSON ApiError where
  parseJSON = Aeson.withObject "ApiError" $ \v -> do
    type_ <- v Aeson..: "error"
    message <- v Aeson..: "message"
    details <- v Aeson..:? "details"
    errors <- v Aeson..:? "errors"
    pure ApiError {..}

-- | Error response from the Extend API
data ErrorResponse = ErrorResponse
  { -- | HTTP status code
    status :: Int,
    -- | Error details
    error :: ApiError
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)