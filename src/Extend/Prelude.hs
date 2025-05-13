-- | Common imports and utilities
module Extend.Prelude
  ( -- * Re-exports
    module Control.Applicative,
    module Data.Aeson,
    module Data.Aeson.Types,
    module Data.ByteString,
    module Data.ByteString.Lazy,
    module Data.Map.Strict,
    module Data.Maybe,
    module Data.Scientific,
    module Data.Text,
    module Data.Text.Encoding,
    module Data.Time,
    module Data.Vector,
    module GHC.Generics,
    module Servant.API,
    module Servant.Client,
    Bool (..),
    Double,
    Either (..),
    IO,
    Int,
    Integer,
    String,
    ($),
    (.),
    Applicative (..),
    Enum (..),
    Eq (..),
    Foldable (..),
    Functor (..),
    Monad (..),
    Monoid (..),
    Num (..),
    Read (..),
    Show (..),
    Traversable (..),
    undefined,

    -- * API token
    ApiToken (..),

    -- * API version
    ApiVersion (..),
    defaultApiVersion,

    -- * Utilities
    tshow,
  )
where

import Control.Applicative ((<|>))
import Data.Aeson
  ( FromJSON (..),
    Options (..),
    ToJSON (..),
    Value (..),
    defaultOptions,
    object,
    withObject,
    (.:),
    (.:?),
    (.=),
  )
import Data.Aeson.Types (Parser)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes, fromMaybe, isJust, maybeToList)
import Data.Proxy (Proxy (..))
import Data.Scientific (Scientific)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time (UTCTime)
import Data.Vector (Vector)
import GHC.Generics (Generic)
import Servant.API
  ( Accept,
    Capture,
    EmptyAPI,
    Get,
    Header',
    JSON,
    NoContent,
    Post,
    QueryParam,
    QueryParams,
    ReqBody,
    Required,
    Strict,
    (:<|>) (..),
    (:>),
  )
import Servant.Client (ClientEnv, ClientM, client)
import Prelude
  ( Applicative (..),
    Bool (..),
    Double,
    Either (..),
    Enum (..),
    Eq (..),
    Foldable (..),
    Functor (..),
    IO,
    Int,
    Integer,
    Monad (..),
    Monoid (..),
    Num (..),
    Read (..),
    Show (..),
    String,
    Traversable (..),
    undefined,
    ($),
    (.),
  )

-- | API token for authentication
newtype ApiToken = ApiToken {unApiToken :: Text}
  deriving (Show, Eq)

-- | API version for Extend API
newtype ApiVersion = ApiVersion {unApiVersion :: Text}
  deriving (Show, Eq)

-- | Default API version - 2025-04-21
defaultApiVersion :: ApiVersion
defaultApiVersion = ApiVersion "2025-04-21"

-- | Convert a value to its 'Text' representation
tshow :: (Show a) => a -> Text
tshow = pack . show