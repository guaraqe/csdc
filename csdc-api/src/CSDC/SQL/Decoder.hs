module CSDC.SQL.Decoder
  ( -- * Base types
    bool
  , bytea
  , int
  , posixTime
  , text
  , textNullable
  , textList
    -- * Local types
  , id
  , messageType
  , messageStatus
  , replyType
  , replyStatus
    -- * Reexport
  , Decoders.rowList
  , Decoders.rowMaybe
  , Decoders.singleRow
  , Decoders.noResult
  ) where

import CSDC.Prelude
import Prelude hiding (id)

import Data.ByteString (ByteString)
import Data.Time.Clock.POSIX (POSIXTime, utcTimeToPOSIXSeconds)
import Hasql.Decoders (Row, column, nonNullable, nullable, listArray)

import qualified Hasql.Decoders as Decoders

--------------------------------------------------------------------------------
-- Base types

bool :: Row Bool
bool = column (nonNullable Decoders.bool)

bytea :: Row ByteString
bytea = column (nonNullable Decoders.bytea)

int :: Row Int
int = fromIntegral <$> column (nonNullable Decoders.int8)

posixTime :: Row POSIXTime
posixTime =
  utcTimeToPOSIXSeconds <$>
  column (nonNullable Decoders.timestamptz)

text :: Row Text
text = column (nonNullable Decoders.text)

textList :: Row [Text]
textList = column (nonNullable (listArray (nonNullable Decoders.text)))

textNullable :: Row (Maybe Text)
textNullable = column (nullable Decoders.text)

--------------------------------------------------------------------------------
-- Local types

id :: Row (Id a)
id = Id <$> column (nonNullable Decoders.uuid)

messageType :: Row MessageType
messageType = column (nonNullable (Decoders.enum decode))
  where
    decode a = lookup a
      [ ("Invitation", Invitation)
      , ("Submission", Submission)
      ]

messageStatus :: Row MessageStatus
messageStatus = column (nonNullable (Decoders.enum decode))
  where
    decode a = lookup a
      [ ("Waiting", Waiting)
      , ("Accepted", Accepted)
      , ("Rejected", Rejected)
      ]

replyType :: Row ReplyType
replyType = column (nonNullable (Decoders.enum decode))
  where
    decode a = lookup a
      [ ("Accept", Accept)
      , ("Reject", Reject)
      ]

replyStatus :: Row ReplyStatus
replyStatus = column (nonNullable (Decoders.enum decode))
  where
    decode a = lookup a
      [ ("Seen", Seen)
      , ("NotSeen", NotSeen)
      ]
