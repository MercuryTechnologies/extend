-- | File operations for the Extend API
module Extend.V1.Files
  ( -- * Types
    File (..),
    FileContents (..),
    Page (..),
    Sheet (..),
    FileMetadata (..),
    ParentSplit (..),
    CreateFileRequest (..),
    CreateFileResponse (..),
    UploadFileResponse (..),
    GetFileResponse (..),
    ListFilesResponse (..),
    DeleteFileResponse (..),

    -- * API
    FilesAPI,
    createFile,
    uploadFile,
    getFile,
    listFiles,
    deleteFile,
  )
where

import qualified Data.Aeson as Aeson
import Data.Proxy (Proxy (..))
import Extend.Prelude
import Extend.V1.Common

-- | Parent split information for a file
data ParentSplit = ParentSplit
  { -- | ID of the split
    id :: Text,
    -- | Type of the split
    type_ :: Text,
    -- | Identifier for the split
    identifier :: Text,
    -- | Starting page of the split
    startPage :: Int,
    -- | Ending page of the split
    endPage :: Int
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ParentSplit where
  parseJSON = withObject "ParentSplit" $ \v -> do
    id <- v .: "id"
    type_ <- v .: "type"
    identifier <- v .: "identifier"
    startPage <- v .: "startPage"
    endPage <- v .: "endPage"
    pure ParentSplit {..}

instance ToJSON ParentSplit where
  toJSON ParentSplit {..} =
    Aeson.object
      [ "id" .= id,
        "type" .= type_,
        "identifier" .= identifier,
        "startPage" .= startPage,
        "endPage" .= endPage
      ]

-- | Metadata for a file
data FileMetadata = FileMetadata
  { -- | Number of pages in the file
    pageCount :: Maybe Int,
    -- | Parent split information if this file is a split
    parentSplit :: Maybe ParentSplit
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON FileMetadata where
  parseJSON = withObject "FileMetadata" $ \v -> do
    pageCount <- v .:? "pageCount"
    parentSplit <- v .:? "parentSplit"
    pure FileMetadata {..}

instance ToJSON FileMetadata where
  toJSON FileMetadata {..} =
    Aeson.object $
      catMaybes
        [ ("pageCount" .=) <$> pageCount,
          ("parentSplit" .=) <$> parentSplit
        ]

-- | Page in a file
data Page = Page
  { -- | The page number
    pageNumber :: Int,
    -- | Height of the page
    pageHeight :: Maybe Int,
    -- | Width of the page
    pageWidth :: Maybe Int,
    -- | Raw text content of the page
    rawText :: Maybe Text,
    -- | Markdown content of the page
    markdown :: Maybe Text,
    -- | HTML content of the page
    html :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON Page where
  parseJSON = withObject "Page" $ \v -> do
    pageNumber <- v .: "pageNumber"
    pageHeight <- v .:? "pageHeight"
    pageWidth <- v .:? "pageWidth"
    rawText <- v .:? "rawText"
    markdown <- v .:? "markdown"
    html <- v .:? "html"
    pure Page {..}

instance ToJSON Page where
  toJSON Page {..} =
    Aeson.object $
      catMaybes
        [ Just ("pageNumber" .= pageNumber),
          ("pageHeight" .=) <$> pageHeight,
          ("pageWidth" .=) <$> pageWidth,
          ("rawText" .=) <$> rawText,
          ("markdown" .=) <$> markdown,
          ("html" .=) <$> html
        ]

-- | Sheet in a file
data Sheet = Sheet
  { -- | Name of the sheet
    sheetName :: Text,
    -- | Raw text content of the sheet
    rawText :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON Sheet where
  parseJSON = withObject "Sheet" $ \v -> do
    sheetName <- v .: "sheetName"
    rawText <- v .:? "rawText"
    pure Sheet {..}

instance ToJSON Sheet where
  toJSON Sheet {..} =
    Aeson.object $
      catMaybes
        [ Just ("sheetName" .= sheetName),
          ("rawText" .=) <$> rawText
        ]

-- | Contents of a file
data FileContents = FileContents
  { -- | Raw text content of the file
    rawText :: Maybe Text,
    -- | Markdown content of the file
    markdown :: Maybe Text,
    -- | Pages in the file
    pages :: Maybe [Page],
    -- | Sheets in the file
    sheets :: Maybe [Sheet]
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON FileContents where
  parseJSON = withObject "FileContents" $ \v -> do
    rawText <- v .:? "rawText"
    markdown <- v .:? "markdown"
    pages <- v .:? "pages"
    sheets <- v .:? "sheets"
    pure FileContents {..}

instance ToJSON FileContents where
  toJSON FileContents {..} =
    Aeson.object $
      catMaybes
        [ ("rawText" .=) <$> rawText,
          ("markdown" .=) <$> markdown,
          ("pages" .=) <$> pages,
          ("sheets" .=) <$> sheets
        ]

-- | A file in the Extend API
data File = File
  { -- | Type of the object
    object :: ObjectType,
    -- | ID of the file
    id :: Text,
    -- | Name of the file
    name :: Text,
    -- | Metadata for the file
    metadata :: FileMetadata,
    -- | When the file was created
    createdAt :: UTCTime,
    -- | When the file was last updated
    updatedAt :: UTCTime,
    -- | Type of the file (PDF, CSV, etc.)
    type_ :: Maybe Text,
    -- | URL to download the file
    presignedUrl :: Maybe Text,
    -- | ID of the parent file if this is a derivative
    parentFileId :: Maybe Text,
    -- | Contents of the file
    contents :: Maybe FileContents
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON File where
  parseJSON = withObject "File" $ \v -> do
    objectType <- v .: "object"
    id <- v .: "id"
    name <- v .: "name"
    metadata <- v .: "metadata"
    createdAt <- v .: "createdAt"
    updatedAt <- v .: "updatedAt"
    type_ <- v .:? "type"
    presignedUrl <- v .:? "presignedUrl"
    parentFileId <- v .:? "parentFileId"
    contents <- v .:? "contents"
    pure
      File
        { object = objectType,
          id = id,
          name = name,
          metadata = metadata,
          createdAt = createdAt,
          updatedAt = updatedAt,
          type_ = type_,
          presignedUrl = presignedUrl,
          parentFileId = parentFileId,
          contents = contents
        }

instance ToJSON File where
  toJSON File {..} =
    Aeson.object $
      catMaybes
        [ Just ("object" .= object),
          Just ("id" .= id),
          Just ("name" .= name),
          Just ("metadata" .= metadata),
          Just ("createdAt" .= createdAt),
          Just ("updatedAt" .= updatedAt),
          ("type" .=) <$> type_,
          ("presignedUrl" .=) <$> presignedUrl,
          ("parentFileId" .=) <$> parentFileId,
          ("contents" .=) <$> contents
        ]

-- | Request to create a file
data CreateFileRequest = CreateFileRequest
  { -- | Name of the file
    name :: Text,
    -- | Type of the file (PDF, CSV, etc.)
    type_ :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON CreateFileRequest where
  toJSON CreateFileRequest {..} =
    Aeson.object $
      catMaybes
        [ Just ("name" .= name),
          ("type" .=) <$> type_
        ]

-- | Response from creating a file
newtype CreateFileResponse = CreateFileResponse
  { -- | The created file
    file :: File
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON CreateFileResponse where
  parseJSON = withObject "CreateFileResponse" $ \v -> do
    file <- v .: "file"
    pure CreateFileResponse {..}

-- | Response from uploading a file
newtype UploadFileResponse = UploadFileResponse
  { -- | Whether the upload was successful
    success :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Response from getting a file
newtype GetFileResponse = GetFileResponse
  { -- | The requested file
    file :: File
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON GetFileResponse where
  parseJSON = withObject "GetFileResponse" $ \v -> do
    file <- v .: "file"
    pure GetFileResponse {..}

-- | Response from listing files
data ListFilesResponse = ListFilesResponse
  { -- | The files
    files :: [File],
    -- | Pagination information
    pagination :: Maybe Pagination
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ListFilesResponse where
  parseJSON = withObject "ListFilesResponse" $ \v -> do
    files <- v .: "files"
    pagination <- v .:? "pagination"
    pure ListFilesResponse {..}

-- | Response from deleting a file
newtype DeleteFileResponse = DeleteFileResponse
  { -- | Whether the deletion was successful
    success :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Files API endpoints
type FilesAPI =
  "files"
    :> Header' '[Required, Strict] "Authorization" Text
    :> Header' '[Required, Strict] "x-extend-api-version" Text
    :> ReqBody '[JSON] CreateFileRequest
    :> Post '[JSON] (SuccessResponse CreateFileResponse)
    :<|> "files"
    :> Capture "fileId" Text
    :> "upload"
    :> Header' '[Required, Strict] "Authorization" Text
    :> Header' '[Required, Strict] "x-extend-api-version" Text
    :> ReqBody '[JSON] Text
    :> Post '[JSON] UploadFileResponse
    :<|> "files"
    :> Capture "fileId" Text
    :> Header' '[Required, Strict] "Authorization" Text
    :> Header' '[Required, Strict] "x-extend-api-version" Text
    :> Get '[JSON] (SuccessResponse GetFileResponse)
    :<|> "files"
    :> Header' '[Required, Strict] "Authorization" Text
    :> Header' '[Required, Strict] "x-extend-api-version" Text
    :> QueryParam "limit" Int
    :> QueryParam "page" Int
    :> Get '[JSON] (SuccessResponse ListFilesResponse)
    :<|> "files"
    :> Capture "fileId" Text
    :> Header' '[Required, Strict] "Authorization" Text
    :> Header' '[Required, Strict] "x-extend-api-version" Text
    :> ReqBody '[JSON] ()
    :> Post '[JSON] DeleteFileResponse

-- | Split the client functions for easier access
filesAPI :: Proxy FilesAPI
filesAPI = Proxy

createFileClient :: Text -> Text -> CreateFileRequest -> ClientM (SuccessResponse CreateFileResponse)
uploadFileClient :: Text -> Text -> Text -> Text -> ClientM UploadFileResponse
getFileClient :: Text -> Text -> Text -> ClientM (SuccessResponse GetFileResponse)
listFilesClient :: Text -> Text -> Maybe Int -> Maybe Int -> ClientM (SuccessResponse ListFilesResponse)
deleteFileClient :: Text -> Text -> Text -> () -> ClientM DeleteFileResponse
createFileClient :<|> uploadFileClient :<|> getFileClient :<|> listFilesClient :<|> deleteFileClient = client filesAPI

-- | Create a new file
createFile ::
  ApiToken ->
  ApiVersion ->
  CreateFileRequest ->
  ClientM (SuccessResponse CreateFileResponse)
createFile (ApiToken token) (ApiVersion version) req =
  createFileClient ("Bearer " <> token) version req

-- | Upload a file
uploadFile ::
  ApiToken ->
  ApiVersion ->
  -- | File ID
  Text ->
  -- | File contents
  Text ->
  ClientM UploadFileResponse
uploadFile (ApiToken token) (ApiVersion version) fileId contents =
  uploadFileClient fileId ("Bearer " <> token) version contents

-- | Get a file
getFile ::
  ApiToken ->
  ApiVersion ->
  -- | File ID
  Text ->
  ClientM (SuccessResponse GetFileResponse)
getFile (ApiToken token) (ApiVersion version) fileId =
  getFileClient fileId ("Bearer " <> token) version

-- | List files
listFiles ::
  ApiToken ->
  ApiVersion ->
  -- | Limit
  Maybe Int ->
  -- | Page
  Maybe Int ->
  ClientM (SuccessResponse ListFilesResponse)
listFiles (ApiToken token) (ApiVersion version) limit page =
  listFilesClient ("Bearer " <> token) version limit page

-- | Delete a file
deleteFile ::
  ApiToken ->
  ApiVersion ->
  -- | File ID
  Text ->
  ClientM DeleteFileResponse
deleteFile (ApiToken token) (ApiVersion version) fileId =
  deleteFileClient fileId ("Bearer " <> token) version ()