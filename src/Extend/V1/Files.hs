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
    parentSplitId :: Text,
    -- | Type of the split
    parentSplitType :: Text,
    -- | Identifier for the split
    parentSplitIdentifier :: Text,
    -- | Starting page of the split
    parentSplitStartPage :: Int,
    -- | Ending page of the split
    parentSplitEndPage :: Int
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ParentSplit where
  parseJSON = withObject "ParentSplit" $ \v -> do
    id <- v .: "id"
    type_ <- v .: "type"
    identifier <- v .: "identifier"
    startPage <- v .: "startPage"
    endPage <- v .: "endPage"
    pure
      ParentSplit
        { parentSplitId = id,
          parentSplitType = type_,
          parentSplitIdentifier = identifier,
          parentSplitStartPage = startPage,
          parentSplitEndPage = endPage
        }

instance ToJSON ParentSplit where
  toJSON ParentSplit {..} =
    Aeson.object
      [ "id" .= parentSplitId,
        "type" .= parentSplitType,
        "identifier" .= parentSplitIdentifier,
        "startPage" .= parentSplitStartPage,
        "endPage" .= parentSplitEndPage
      ]

-- | Metadata for a file
data FileMetadata = FileMetadata
  { -- | Number of pages in the file
    fileMetadataPageCount :: Maybe Int,
    -- | Parent split information if this file is a split
    fileMetadataParentSplit :: Maybe ParentSplit
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON FileMetadata where
  parseJSON = withObject "FileMetadata" $ \v -> do
    pageCount <- v .:? "pageCount"
    parentSplit <- v .:? "parentSplit"
    pure
      FileMetadata
        { fileMetadataPageCount = pageCount,
          fileMetadataParentSplit = parentSplit
        }

instance ToJSON FileMetadata where
  toJSON FileMetadata {..} =
    Aeson.object $
      catMaybes
        [ ("pageCount" .=) <$> fileMetadataPageCount,
          ("parentSplit" .=) <$> fileMetadataParentSplit
        ]

-- | Page in a file
data Page = Page
  { -- | The page number
    pageNumber :: Int,
    -- | Height of the page
    pageHeight :: Maybe Double,
    -- | Width of the page
    pageWidth :: Maybe Double,
    -- | Raw text content of the page
    pageRawText :: Maybe Text,
    -- | Markdown content of the page
    pageMarkdown :: Maybe Text,
    -- | HTML content of the page
    pageHtml :: Maybe Text
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
    pure
      Page
        { pageNumber = pageNumber,
          pageHeight = pageHeight,
          pageWidth = pageWidth,
          pageRawText = rawText,
          pageMarkdown = markdown,
          pageHtml = html
        }

instance ToJSON Page where
  toJSON Page {..} =
    Aeson.object $
      catMaybes
        [ Just ("pageNumber" .= pageNumber),
          ("pageHeight" .=) <$> pageHeight,
          ("pageWidth" .=) <$> pageWidth,
          ("rawText" .=) <$> pageRawText,
          ("markdown" .=) <$> pageMarkdown,
          ("html" .=) <$> pageHtml
        ]

-- | Sheet in a file
data Sheet = Sheet
  { -- | Name of the sheet
    sheetName :: Text,
    -- | Raw text content of the sheet
    sheetRawText :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON Sheet where
  parseJSON = withObject "Sheet" $ \v -> do
    sheetName <- v .: "sheetName"
    rawText <- v .:? "rawText"
    pure
      Sheet
        { sheetName = sheetName,
          sheetRawText = rawText
        }

instance ToJSON Sheet where
  toJSON Sheet {..} =
    Aeson.object $
      catMaybes
        [ Just ("sheetName" .= sheetName),
          ("rawText" .=) <$> sheetRawText
        ]

-- | Contents of a file
data FileContents = FileContents
  { -- | Raw text content of the file
    fileContentsRawText :: Maybe Text,
    -- | Markdown content of the file
    fileContentsMarkdown :: Maybe Text,
    -- | Pages in the file
    fileContentsPages :: Maybe [Page],
    -- | Sheets in the file
    fileContentsSheets :: Maybe [Sheet]
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON FileContents where
  parseJSON = withObject "FileContents" $ \v -> do
    rawText <- v .:? "rawText"
    markdown <- v .:? "markdown"
    pages <- v .:? "pages"
    sheets <- v .:? "sheets"
    pure
      FileContents
        { fileContentsRawText = rawText,
          fileContentsMarkdown = markdown,
          fileContentsPages = pages,
          fileContentsSheets = sheets
        }

instance ToJSON FileContents where
  toJSON FileContents {..} =
    Aeson.object $
      catMaybes
        [ ("rawText" .=) <$> fileContentsRawText,
          ("markdown" .=) <$> fileContentsMarkdown,
          ("pages" .=) <$> fileContentsPages,
          ("sheets" .=) <$> fileContentsSheets
        ]

-- | A file in the Extend API
data File = File
  { -- | Type of the object
    fileObject :: ObjectType,
    -- | ID of the file
    fileId :: Text,
    -- | Name of the file
    fileName :: Text,
    -- | Metadata for the file
    fileMetadata :: FileMetadata,
    -- | When the file was created
    fileCreatedAt :: UTCTime,
    -- | When the file was last updated
    fileUpdatedAt :: UTCTime,
    -- | Type of the file (PDF, CSV, etc.)
    fileType :: Maybe Text,
    -- | URL to download the file
    filePresignedUrl :: Maybe Text,
    -- | ID of the parent file if this is a derivative
    fileParentFileId :: Maybe Text,
    -- | Contents of the file
    fileContents :: Maybe FileContents
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
        { fileObject = objectType,
          fileId = id,
          fileName = name,
          fileMetadata = metadata,
          fileCreatedAt = createdAt,
          fileUpdatedAt = updatedAt,
          fileType = type_,
          filePresignedUrl = presignedUrl,
          fileParentFileId = parentFileId,
          fileContents = contents
        }

instance ToJSON File where
  toJSON File {..} =
    Aeson.object $
      catMaybes
        [ Just ("object" .= fileObject),
          Just ("id" .= fileId),
          Just ("name" .= fileName),
          Just ("metadata" .= fileMetadata),
          Just ("createdAt" .= fileCreatedAt),
          Just ("updatedAt" .= fileUpdatedAt),
          ("type" .=) <$> fileType,
          ("presignedUrl" .=) <$> filePresignedUrl,
          ("parentFileId" .=) <$> fileParentFileId,
          ("contents" .=) <$> fileContents
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
    createFileResponseFile :: File
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON CreateFileResponse where
  parseJSON = withObject "CreateFileResponse" $ \v -> do
    file <- v .: "file"
    pure CreateFileResponse {createFileResponseFile = file}

-- | Response from uploading a file
newtype UploadFileResponse = UploadFileResponse
  { -- | Whether the upload was successful
    uploadFileResponseSuccess :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Response from getting a file
data GetFileResponse = GetFileResponse
  { -- | Whether the request was successful
    getFileResponseSuccess :: Bool,
    -- | The requested file
    getFileResponseFile :: File
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON GetFileResponse where
  parseJSON = withObject "GetFileResponse" $ \v -> do
    success <- v .: "success"
    file <- v .: "file"
    pure GetFileResponse {getFileResponseSuccess = success, getFileResponseFile = file}

instance ToJSON GetFileResponse where
  toJSON GetFileResponse {..} =
    Aeson.object
      [ "success" .= getFileResponseSuccess,
        "file" .= getFileResponseFile
      ]

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
    :> QueryParam "rawText" Bool
    :> QueryParam "markdown" Bool
    :> QueryParam "html" Bool
    :> Get '[JSON] GetFileResponse
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
getFileClient :: Text -> Text -> Text -> Maybe Bool -> Maybe Bool -> Maybe Bool -> ClientM GetFileResponse
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
  -- | Include raw text content
  Maybe Bool ->
  -- | Include markdown content
  Maybe Bool ->
  -- | Include HTML content
  Maybe Bool ->
  ClientM GetFileResponse
getFile (ApiToken token) (ApiVersion version) fileId rawText markdown html =
  getFileClient fileId ("Bearer " <> token) version rawText markdown html

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