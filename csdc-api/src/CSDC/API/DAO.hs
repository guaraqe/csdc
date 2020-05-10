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

--------------------------------------------------------------------------------
-- CRUD and REL API

type CRUD (name :: Symbol) a =
       name :> Capture "id" (Id a) :> GetJSON (Maybe a)
  :<|> name :> PostJSON a (Id a)
  :<|> name :> Capture "id" (Id a) :> PostJSON a ()
  :<|> name :> Capture "id" (Id a) :> DeleteJSON ()

serveCRUD :: HasCRUD a m => ServerT (CRUD name a) m
serveCRUD =
       select
  :<|> insert
  :<|> update
  :<|> delete

type REL (name :: Symbol) (left :: Symbol) (right :: Symbol) r =
       name :> left :> Capture "id" (Id (RelationL r)) :> GetJSON (IdMap r r)
  :<|> name :> right :> Capture "id" (Id (RelationR r)) :> GetJSON (IdMap r r)
  :<|> name :> PostJSON r (Id r)
  :<|> name :> Capture "id" (Id r) :> DeleteJSON ()

serveREL ::
  (IsRelation r, HasRelation r m) => ServerT (REL name left right r) m
serveREL =
       selectRelationL
  :<|> selectRelationR
  :<|> insertRelation
  :<|> deleteRelation

--------------------------------------------------------------------------------
-- Person API

type PersonAPI =
       "person" :> "root" :> GetJSON UserId
  :<|> "person" :> Capture "id" (Id Person) :> "units" :> GetJSON (IdMap Member Unit)
  :<|> CRUD "person" Person

servePersonAPI :: (HasUser m, HasDAO m) => ServerT PersonAPI m
servePersonAPI =
       getUser
  :<|> getUserUnits
  :<|> serveCRUD

--------------------------------------------------------------------------------
-- Unit API

type UnitAPI =
       "unit" :> "root" :> Get '[JSON] (Id Unit)
  :<|> "unit" :> Capture "id" (Id Unit) :> "members" :> GetJSON (IdMap Member (WithId Person))
  :<|> "unit" :> Capture "id" (Id Unit) :> "subparts" :> GetJSON (IdMap Subpart (WithId Unit))
  :<|> "unit" :> "create" :> PostJSON (Id Person) (WithId Member)
  :<|> CRUD "unit" Unit

serveUnitAPI :: HasDAO m => ServerT UnitAPI m
serveUnitAPI =
       rootUnit
  :<|> getUnitMembers
  :<|> getUnitSubparts
  :<|> createUnit
  :<|> serveCRUD

--------------------------------------------------------------------------------
-- Member API

type MemberAPI = REL "member" "person" "unit" Member

serveMemberAPI :: HasDAO m => ServerT MemberAPI m
serveMemberAPI = serveREL

--------------------------------------------------------------------------------
-- Subpart API

type SubpartAPI = REL "subpart" "child" "parent" Subpart

serveSubpartAPI :: HasDAO m => ServerT SubpartAPI m
serveSubpartAPI = serveREL

--------------------------------------------------------------------------------
-- API

type API = PersonAPI :<|> UnitAPI :<|> MemberAPI :<|> SubpartAPI

serveAPI :: (HasUser m, HasDAO m) => ServerT API m
serveAPI =
       servePersonAPI
  :<|> serveUnitAPI
  :<|> serveMemberAPI
  :<|> serveSubpartAPI
