module CSDC.SQL.Encoder
  ( -- * Base types
    bool,
    bytea,
    int,
    posixTime,
    text,
    textNullable,
    textList,

    -- * Local types
    id,
    idNullable,
    idList,
    messageType,
    messageStatus,
    replyType,
    replyStatus,
    electionChoiceList,
    electionType,
    votePayload,
  )
where

import CSDC.Prelude
import CSDC.Types.Election
import Data.Aeson qualified as JSON
import Data.ByteString (ByteString)
import Data.Functor.Contravariant (Contravariant (..))
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime)
import Hasql.Encoders
  ( Params,
    foldableArray,
    nonNullable,
    nullable,
    param,
  )
import Hasql.Encoders qualified as Encoders
import Prelude hiding (id)

--------------------------------------------------------------------------------
-- Base types

bool :: Params Bool
bool = param (nonNullable Encoders.bool)

bytea :: Params ByteString
bytea = param (nonNullable Encoders.bytea)

int :: Params Int
int = contramap fromIntegral $ param (nonNullable Encoders.int8)

posixTime :: Params POSIXTime
posixTime =
  contramap posixSecondsToUTCTime $
    param (nonNullable Encoders.timestamptz)

text :: Params Text
text = param (nonNullable Encoders.text)

textNullable :: Params (Maybe Text)
textNullable = param (nullable Encoders.text)

textList :: Params [Text]
textList = param $ nonNullable $ foldableArray $ nonNullable Encoders.text

jsonb :: Params JSON.Value
jsonb = param (nonNullable Encoders.jsonb)

--------------------------------------------------------------------------------
-- Local types

id :: Params (Id a)
id =
  contramap getId $
    param (nonNullable Encoders.uuid)

idNullable :: Params (Maybe (Id a))
idNullable =
  contramap (fmap getId) $
    param (nullable Encoders.uuid)

idList :: Params [Id a]
idList =
  contramap (fmap getId) $
    param (nonNullable (foldableArray (nonNullable Encoders.uuid)))

messageType :: Params MessageType
messageType = contramap encode text
  where
    encode Invitation = "Invitation"
    encode Submission = "Submission"

messageStatus :: Params MessageStatus
messageStatus = contramap encode text
  where
    encode Waiting = "Waiting"
    encode Accepted = "Accepted"
    encode Rejected = "Rejected"

replyType :: Params ReplyType
replyType = contramap encode text
  where
    encode Accept = "Accept"
    encode Reject = "Reject"

replyStatus :: Params ReplyStatus
replyStatus = contramap encode text
  where
    encode Seen = "Seen"
    encode NotSeen = "NotSeen"

electionType :: Params ElectionType
electionType = contramap encode text
  where
    encode SimpleMajority = "SimpleMajority"
    encode MajorityConsensus = "MajorityConsensus"

electionChoiceList :: Params [ElectionChoice]
electionChoiceList = contramap (fmap (.getElectionChoice)) textList

votePayload :: Params VotePayload
votePayload = contramap JSON.toJSON jsonb
