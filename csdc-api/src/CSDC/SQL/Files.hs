{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CSDC.SQL.Files
  ( selectFile
  , selectFileContents
  , upsertFile
  , updateFileIPFS
  , selectFolderFiles
  , selectFolderSubfolders
  ) where

import CSDC.Prelude
import CSDC.Types.File (FileDB (..), NewFileDB (..))

import qualified CSDC.SQL.Decoder as Decoder
import qualified CSDC.SQL.Encoder as Encoder

import Data.ByteString (ByteString)
import Data.Functor.Contravariant (Contravariant (..))
import Hasql.Statement (Statement (..))

import qualified Data.ByteString.Char8 as ByteString
import qualified Hasql.Decoders as Decoders

--------------------------------------------------------------------------------
-- Files

selectFile :: Statement (Text,Text) (Maybe FileDB)
selectFile = Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "SELECT folder, name, size, hash, ipfs, modified_at"
      , "FROM files"
      , "WHERE folder = $1 AND name = $2"
      ]

    encoder =
      contramap fst Encoder.text <>
      contramap snd Encoder.text

    decoder = Decoder.rowMaybe $ do
      fileDB_folder <- Decoder.text
      fileDB_name <- Decoder.text
      fileDB_size <- Decoder.int
      fileDB_hash <- Decoder.bytea
      fileDB_ipfs <- Decoder.textNullable
      fileDB_modifiedAt <- Decoder.posixTime
      pure FileDB {..}

selectFileContents :: Statement (Text,Text) ByteString
selectFileContents = Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "SELECT contents"
      , "FROM files"
      , "WHERE folder = $1 AND name = $2"
      ]

    encoder =
      contramap fst Encoder.text <>
      contramap snd Encoder.text

    decoder = Decoder.singleRow Decoder.bytea

upsertFile :: Statement NewFileDB ()
upsertFile = Statement sql encoder Decoders.noResult True
  where
    sql = ByteString.unlines
      [ "INSERT INTO files (folder,name,contents,size,hash,modified_at)"
      , "VALUES ($1,$2,$3,$4,$5,NOW())"
      , "ON CONFLICT (folder,name)"
      , "DO UPDATE SET contents = $3, size = $4, hash = $5, modified_at = NOW()"
      ]

    encoder =
      contramap newFileDB_folder Encoder.text <>
      contramap newFileDB_name Encoder.text <>
      contramap newFileDB_contents Encoder.bytea <>
      contramap newFileDB_size Encoder.int <>
      contramap newFileDB_hash Encoder.bytea

updateFileIPFS :: Statement (ByteString, Text) ()
updateFileIPFS = Statement sql encoder Decoders.noResult True
  where
    sql = ByteString.unlines
      [ "UPDATE files "
      , "SET ipfs = $2"
      , "WHERE hash = $1"
      ]

    encoder =
      contramap fst Encoder.bytea <>
      contramap snd Encoder.text

--------------------------------------------------------------------------------
-- Folders

selectFolderFiles :: Statement Text [FileDB]
selectFolderFiles = Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "SELECT folder, name, size, hash, ipfs, modified_at"
      , "FROM files"
      , "WHERE folder = $1"
      , "ORDER BY name"
      ]

    encoder = Encoder.text

    decoder = Decoder.rowList $ do
      fileDB_folder <- Decoder.text
      fileDB_name <- Decoder.text
      fileDB_size <- Decoder.int
      fileDB_hash <- Decoder.bytea
      fileDB_ipfs <- Decoder.textNullable
      fileDB_modifiedAt <- Decoder.posixTime
      pure FileDB {..}

selectFolderSubfolders :: Statement Text [Text]
selectFolderSubfolders = Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "SELECT DISTINCT ON (result)"
      , "regexp_replace(regexp_replace(folder, $1, ''), '/(.)+', '') as result"
      , "FROM files"
      , "WHERE folder LIKE $2"
      , "ORDER BY result"
      ]

    toFolder t = t <> "/"

    toLike t = t <> "/%"

    encoder =
      contramap toFolder Encoder.text <>
      contramap toLike Encoder.text

    decoder = Decoder.rowList Decoder.text

