module CSDC.API.DAO
  ( API
  , serveAPI
  ) where

import CSDC.Prelude hiding (JSON)

import GHC.Types (Symbol)
import Servant

--------------------------------------------------------------------------------
-- Synonyms

type GetJSON a = Get '[JSON] a
type PostJSON a b = ReqBody '[JSON] a :> Post '[JSON] b
type DeleteJSON a = Delete '[JSON] a
type CaptureId a = Capture "id" (Id a)

--------------------------------------------------------------------------------
-- CRUD and REL API

type CRUD a =
       CaptureId a :> GetJSON (Maybe a)
  :<|> PostJSON a (Id a)
  :<|> CaptureId a :> PostJSON a ()
  :<|> CaptureId a :> DeleteJSON ()

serveCRUD :: HasCRUD a m => ServerT (CRUD a) m
serveCRUD =
       select
  :<|> insert
  :<|> update
  :<|> delete

type REL (left :: Symbol) (right :: Symbol) r =
       left :> CaptureId (RelationL r) :> GetJSON (IdMap r r)
  :<|> right :> CaptureId (RelationR r) :> GetJSON (IdMap r r)
  :<|> PostJSON r (Id r)
  :<|> CaptureId r :> DeleteJSON ()

serveREL ::
  (IsRelation r, HasRelation r m) => ServerT (REL left right r) m
serveREL =
       selectRelationL
  :<|> selectRelationR
  :<|> insertRelation
  :<|> deleteRelation

type MSG r =
       "send" :> PostJSON (Message r) ()
  :<|> "reply" :> PostJSON (Reply r) ()
  :<|> "view" :> PostJSON (Id (Reply r)) ()

serveMSG :: HasMessage r m => ServerT (MSG r) m
serveMSG =
       sendMessage
  :<|> sendReply
  :<|> viewReply

--------------------------------------------------------------------------------
-- Person API

type PersonAPI =
       "root" :> GetJSON UserId
  :<|> CaptureId Person :> "units" :> GetJSON (IdMap Member Unit)
  :<|> CRUD Person

servePersonAPI :: (HasUser m, HasDAO m) => ServerT PersonAPI m
servePersonAPI =
       getUser
  :<|> getUserUnits
  :<|> serveCRUD

--------------------------------------------------------------------------------
-- Unit API

type UnitAPI =
       "root" :> Get '[JSON] (Id Unit)
  :<|> CaptureId Unit :> "members" :> GetJSON (IdMap Member (WithId Person))
  :<|> CaptureId Unit :> "children" :> GetJSON (IdMap Subpart (WithId Unit))
  :<|> CaptureId Unit :> "parents" :> GetJSON (IdMap Subpart (WithId Unit))
  :<|> "create" :> PostJSON (Id Person) (WithId Member)
  :<|> CRUD Unit

serveUnitAPI :: HasDAO m => ServerT UnitAPI m
serveUnitAPI =
       rootUnit
  :<|> getUnitMembers
  :<|> getUnitChildren
  :<|> getUnitParents
  :<|> createUnit
  :<|> serveCRUD

--------------------------------------------------------------------------------
-- Member API

type MemberAPI = REL "person" "unit" Member

serveMemberAPI :: HasDAO m => ServerT MemberAPI m
serveMemberAPI = serveREL

--------------------------------------------------------------------------------
-- Subpart API

type SubpartAPI = REL "child" "parent" Subpart

serveSubpartAPI :: HasDAO m => ServerT SubpartAPI m
serveSubpartAPI = serveREL

--------------------------------------------------------------------------------
-- Message API

type MessageAPI =
       "member" :> MSG Member
  :<|> "subpart" :> MSG Subpart

serveMessageAPI :: HasDAO m => ServerT MessageAPI m
serveMessageAPI =
       serveMSG
  :<|> serveMSG

--------------------------------------------------------------------------------
-- API

type API =
       "person" :> PersonAPI
  :<|> "unit" :> UnitAPI
  :<|> "member" :> MemberAPI
  :<|> "subpart" :> SubpartAPI
  :<|> "message" :> MessageAPI

serveAPI :: (HasUser m, HasDAO m) => ServerT API m
serveAPI =
       servePersonAPI
  :<|> serveUnitAPI
  :<|> serveMemberAPI
  :<|> serveSubpartAPI
  :<|> serveMessageAPI
