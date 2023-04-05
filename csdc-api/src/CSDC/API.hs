{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module CSDC.API
  ( API,
    serveAPI,
  )
where

import CSDC.API.Auth qualified as Auth
import CSDC.Action
import CSDC.FileServer (serveSQLFileServer)
import CSDC.SQL qualified as SQL
import Servant hiding (Server)
import WaiAppStatic.Storage.Filesystem (defaultWebAppSettings)
import WaiAppStatic.Types (StaticSettings (..), unsafeToPiece)
import GHC.Generics (Generic)

--------------------------------------------------------------------------------
-- API

data API mode = API
  { authAPI :: mode :- NamedRoutes Auth.API
  , filesAPI :: mode :- "files" :> Raw
  , rawAPI :: mode :- Raw
  }
  deriving (Generic)

serveAPI :: FilePath -> SQL.Context -> Auth.Settings -> Server API
serveAPI path ctx settings = API
  { authAPI = Auth.serveAPI settings
  , filesAPI = serveSQLFileServer ctx
  , rawAPI = serveDirectoryWith (options path)
  }

options :: FilePath -> StaticSettings
options path =
  let base = defaultWebAppSettings path

      indexRedirect old = \case
        [] -> old [unsafeToPiece "index.html"]
        pcs -> old pcs
   in base {ssLookupFile = indexRedirect (ssLookupFile base)}
