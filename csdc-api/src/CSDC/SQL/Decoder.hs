module CSDC.SQL.Decoder
  ( -- * Base types
    bool,
    bytea,
    int,
    posixTime,
    posixTimeNullable,
    text,
    textNullable,
    textList,

    -- * Local types
    id,
    messageType,
    messageStatus,
    replyType,
    replyStatus,
    electionChoiceList,
    electionChoiceNullable,
    electionType,
    votePayload,

    -- * Reexport
    Decoders.rowList,
    Decoders.rowMaybe,
    Decoders.singleRow,
    Decoders.noResult,
  )
where

import CSDC.Prelude
import CSDC.Types.Election
import Data.Aeson qualified as JSON
import Data.ByteString (ByteString)
import Data.Text qualified as Text
import Data.Time.Clock.POSIX (POSIXTime, utcTimeToPOSIXSeconds)
import Hasql.Decoders (Row, column, listArray, nonNullable, nullable)
import Hasql.Decoders qualified as Decoders
import Prelude hiding (id)

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
  utcTimeToPOSIXSeconds
    <$> column (nonNullable Decoders.timestamptz)

posixTimeNullable :: Row (Maybe POSIXTime)
posixTimeNullable =
  fmap utcTimeToPOSIXSeconds
    <$> column (nullable Decoders.timestamptz)

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
    decode a =
      lookup
        a
        [ ("Invitation", Invitation),
          ("Submission", Submission)
        ]

messageStatus :: Row MessageStatus
messageStatus = column (nonNullable (Decoders.enum decode))
  where
    decode a =
      lookup
        a
        [ ("Waiting", Waiting),
          ("Accepted", Accepted),
          ("Rejected", Rejected)
        ]

replyType :: Row ReplyType
replyType = column (nonNullable (Decoders.enum decode))
  where
    decode a =
      lookup
        a
        [ ("Accept", Accept),
          ("Reject", Reject)
        ]

replyStatus :: Row ReplyStatus
replyStatus = column (nonNullable (Decoders.enum decode))
  where
    decode a =
      lookup
        a
        [ ("Seen", Seen),
          ("NotSeen", NotSeen)
        ]

electionChoiceList :: Row [ElectionChoice]
electionChoiceList = fmap ElectionChoice <$> textList

electionChoiceNullable :: Row (Maybe ElectionChoice)
electionChoiceNullable = fmap ElectionChoice <$> textNullable

electionType :: Row ElectionType
electionType = column (nonNullable (Decoders.enum decode))
  where
    decode a =
      lookup
        a
        [ ("SimpleMajority", SimpleMajority),
          ("MajorityConsensus", MajorityConsensus)
        ]

votePayload :: Row VotePayload
votePayload = column (nonNullable (Decoders.refine fromJSON Decoders.jsonb))
  where
    fromJSON a = case JSON.fromJSON a of
      JSON.Error e -> Left (Text.pack e)
      JSON.Success s -> Right s
