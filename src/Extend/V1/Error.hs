-- | Error types and handling for the Extend API
module Extend.V1.Error
  ( -- * Error types
    ErrorResponse (..),
  )
where

import qualified Data.Aeson as Aeson
import Extend.Prelude

-- | Error response from the Extend API
data ErrorResponse = ErrorResponse
  { -- | Whether the request was successful
    extendApiErrorSuccess :: Bool,
    -- | Error message, if any
    extendApiErrorMessage :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

instance FromJSON ErrorResponse where
  parseJSON = Aeson.withObject "ErrorResponse" $ \v -> do
    extendApiErrorSuccess <- v Aeson..: "success"
    extendApiErrorMessage <- v Aeson..: "error"
    pure ErrorResponse {..}