{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module CSDC.API.Auth
  ( API,
    serveAPI,
    Settings (..),
    makeSettings,
    makeContext,
    contextProxy,
  )
where

import CSDC.API.DAO qualified as DAO
import CSDC.Action hiding (Context)
import CSDC.DAO (createUser)
import CSDC.Prelude hiding (Post)
import CSDC.SQL.Persons qualified as SQL.Persons
import Data.Password.Bcrypt (PasswordCheck (..), checkPassword, mkPassword)
import Servant hiding (Server, Unauthorized, throwError)
import Servant.Auth.Server
  ( Auth,
    AuthResult (..),
    Cookie,
    CookieSettings (..),
    FromJWT,
    JWTSettings,
    SetCookie,
    ToJWT,
    acceptLogin,
    clearSession,
    defaultCookieSettings,
    defaultJWTSettings,
    generateKey,
    writeKey,
    readKey,
  )
import Servant.Server.Generic (AsServerT)
import System.Directory (doesFileExist)

--------------------------------------------------------------------------------
-- User

newtype User = User {getUser :: Id Person}
  deriving (Eq, Show, Generic)
  deriving newtype (FromJSON, ToJSON)

instance ToJWT User

instance FromJWT User

--------------------------------------------------------------------------------
-- Login

data Login = Login {email :: !Text, password :: !Text}
  deriving (Eq, Show, Generic)

instance ToJSON Login

instance FromJSON Login

--------------------------------------------------------------------------------
-- Signin API

type CookieHeaders =
  Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie]

data SigninAPI mode = SigninAPI
  { signin :: mode :- ReqBody '[JSON] Login :> Verb 'POST 204 '[JSON] (CookieHeaders NoContent)
  }
  deriving (Generic)

serveSigninAPI :: Settings -> Server SigninAPI
serveSigninAPI settings =
  SigninAPI
    { signin = authenticate settings
    }

authenticate :: Settings -> Login -> Action () (CookieHeaders NoContent)
authenticate (Settings cookieSettings jwtSettings) (Login email password) =
  runQuery SQL.Persons.check email >>= \case
    Nothing ->
      throw Unauthorized
    Just (personId, passwordHash) -> do
      case checkPassword (mkPassword password) passwordHash of
        PasswordCheckFail ->
          throw Unauthorized
        PasswordCheckSuccess -> do
          mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings (User personId)
          case mApplyCookies of
            Nothing ->
              throw Unauthorized
            Just applyCookies ->
              return $ applyCookies NoContent

--------------------------------------------------------------------------------
-- Signup API

data SignupAPI mode = SignupAPI
  { signup :: mode :- ReqBody '[JSON] NewUser :> Post '[JSON] (Id Person)
  }
  deriving (Generic)

serveSignupAPI :: Server SignupAPI
serveSignupAPI =
  SignupAPI
    { signup = createUser
    }

--------------------------------------------------------------------------------
-- Signout API

data SignoutAPI mode = SignoutAPI
  { signout :: mode :- Get '[JSON] (CookieHeaders Text)
  }
  deriving (Generic)

serveSignoutAPI :: Monad m => Settings -> SignoutAPI (AsServerT m)
serveSignoutAPI (Settings {..}) =
  SignoutAPI
    { signout = return $ clearSession settingsCookie ""
    }

--------------------------------------------------------------------------------
-- API with auth

serveAuthAPI :: AuthResult User -> Server DAO.NamedAPI
serveAuthAPI (Authenticated (User personId)) =
  hoistServer (Proxy @DAO.API) (withPerson personId) DAO.serveAPI
serveAuthAPI _ = throwUnauthorized

data API mode = API
  { daoAPI :: mode :- "api" :> Auth '[Cookie] User :> DAO.API,
    signinAPI :: mode :- "signin" :> NamedRoutes SigninAPI,
    signupAPI :: mode :- "signup" :> NamedRoutes SignupAPI,
    signoutAPI :: mode :- "signout" :> NamedRoutes SignoutAPI
  }
  deriving (Generic)

data Settings = Settings
  { settingsCookie :: CookieSettings,
    settingsJWT :: JWTSettings
  }

makeSettings :: Maybe FilePath -> IO Settings
makeSettings mpath = do
  key <- case mpath of
    Nothing -> generateKey
    Just path -> do
      doesFileExist path >>= \case
        False ->
          writeKey path
        True ->
          pure ()
      readKey path
  pure
    Settings
      { settingsCookie = defaultCookieSettings {cookieXsrfSetting = Nothing},
        settingsJWT = defaultJWTSettings key
      }

serveAPI :: Settings -> Server API
serveAPI settings =
  API
    { daoAPI = serveAuthAPI,
      signinAPI = serveSigninAPI settings,
      signupAPI = serveSignupAPI,
      signoutAPI = serveSignoutAPI settings
    }

contextProxy :: Proxy '[CookieSettings, JWTSettings]
contextProxy = Proxy

makeContext :: Settings -> Context '[CookieSettings, JWTSettings]
makeContext Settings {..} =
  settingsCookie :. settingsJWT :. EmptyContext

--------------------------------------------------------------------------------
-- Throw All

class ThrowUnauthorized a where
  throwUnauthorized :: a

instance
  (ThrowUnauthorized a, ThrowUnauthorized b) =>
  ThrowUnauthorized (a :<|> b)
  where
  throwUnauthorized = throwUnauthorized :<|> throwUnauthorized

instance
  {-# OVERLAPPING #-}
  ThrowUnauthorized b =>
  ThrowUnauthorized (a -> b)
  where
  throwUnauthorized = const throwUnauthorized

instance {-# OVERLAPPABLE #-} ThrowUnauthorized (Action user a) where
  throwUnauthorized = throw Unauthorized

instance
  {-# OVERLAPPABLE #-}
  ( ThrowUnauthorized (ToServant routes mode),
    GenericServant routes mode
  ) =>
  ThrowUnauthorized (routes mode)
  where
  throwUnauthorized = fromServant throwUnauthorized
