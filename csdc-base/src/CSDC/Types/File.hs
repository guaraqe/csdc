{-# LANGUAGE OverloadedStrings #-}

module CSDC.Types.File
  ( Base64File (..)
  , base64FileFromByteString
  , File (..)
  , fromBase64File
  , NewFileDB (..)
  , toNewFileDB
  , FileDB (..)
  , FileUI (..)
  ) where

import CSDC.Aeson (JSON (..))

import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (ToJSON, FromJSON)
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Time.Clock.POSIX (POSIXTime)
import GHC.Generics (Generic)

import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text as Text

-- | A file encoded in base 64, as encoded in a HTML canvas, with name.
data Base64File = Base64File
  { base64File_name :: Text
  , base64File_contents :: Text
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON Base64File

base64FileFromByteString :: Text -> Lazy.ByteString -> Base64File
base64FileFromByteString name contents = Base64File
  { base64File_name = name
  , base64File_contents = "image/svg+xml;base64," <>
      decodeUtf8 (Base64.encode (Lazy.toStrict contents))
  }

parseBase64Contents :: Text -> ByteString
parseBase64Contents =
  Base64.decodeLenient .
  encodeUtf8 .
  fromMaybe (error "Could not extract base 64 prefix.") .
  Text.stripPrefix ";base64," .
  Text.dropWhile (/= ';')

-- | A file, with its name and contents.
data File = File
  { file_name :: Text
  , file_contents :: ByteString
  } deriving (Show, Eq)

fromBase64File :: Base64File -> File
fromBase64File (Base64File name contents) = File
  { file_name = name
  , file_contents = parseBase64Contents contents
  }

-- | A file to be stored in the database.
data NewFileDB = NewFileDB
  { newFileDB_folder :: Text
  , newFileDB_name :: Text
  , newFileDB_contents :: ByteString
  , newFileDB_size :: Int
  , newFileDB_hash :: ByteString
  } deriving (Show, Eq)

toNewFileDB :: MonadIO m => Text -> File -> m NewFileDB
toNewFileDB folder (File name contents) = do
  pure NewFileDB
    { newFileDB_folder = folder
    , newFileDB_name = name
    , newFileDB_contents = contents
    , newFileDB_size = ByteString.length contents
    , newFileDB_hash = MD5.hash contents
    }

-- | A file as stored in the database. It doesn't contain the contents so that
-- file listings are efficient.
data FileDB = FileDB
  { fileDB_folder :: Text
  , fileDB_name :: Text
  , fileDB_size :: Int
  , fileDB_hash :: ByteString
  , fileDB_ipfs :: Maybe Text
  , fileDB_modifiedAt :: POSIXTime
  } deriving (Show, Eq)

-- | A file displayed in the UI.
data FileUI = FileUI
  { fileUI_path :: Text
  , fileUI_name :: Text
  , fileUI_size :: Int
  , fileUI_ipfs :: Maybe Text
  , fileUI_modifiedAt :: POSIXTime
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON FileUI
