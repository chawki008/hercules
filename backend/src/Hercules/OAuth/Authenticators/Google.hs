{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

{-|
Google specific OAuth2 functionality
-}
module Hercules.OAuth.Authenticators.Google
  ( googleAuthenticator
  , findUser
  ) where

import Control.Concurrent.Async (concurrently)
import Control.Monad.Except
import Data.Aeson.TH
import Data.Maybe               (fromJust)
import Data.Text
import Data.Text.Encoding
import Network.HTTP.Client      (Manager)
import Network.OAuth.OAuth2     hiding (URI)
import Network.URI
import Prelude              hiding (id)
import Hercules.Config      (AuthClientInfo (..))
import Hercules.OAuth.Types
import Hercules.OAuth.User
import Hercules.ServerEnv
import Hercules.Query.Hercules
import Hercules.Database.Hercules
import Hercules.Encryption
import Hercules.Log
import Data.Semigroup
import Control.Monad.Log
import Servant      hiding (QueryParams)
import Hercules.Helpers.Helpers 
import Opaleye            

{-# ANN module ("HLint: Ignore Use CamelCase" :: String) #-}

data GoogleToken = GoogleToken
  { audience   :: Text
  , scope      :: Text
  , userid     :: Maybe Text
  , expires_in :: Integer
  }
  deriving (Show)

deriveJSON defaultOptions ''GoogleToken

data GoogleUser = GoogleUser
  { id             :: Text
  , email          :: Text
  , verified_email :: Bool
  , name           :: Text
  }
  deriving (Show, Eq)

deriveJSON defaultOptions ''GoogleUser

googleAuthenticator
  :: (AuthenticatorName -> URI)
  -> AuthClientInfo
  -> OAuth2Authenticator App
googleAuthenticator makeCallback clientInfo =
  makeAuthenticator makeCallback
                    (AuthenticatorName "google")
                    googleScopeEmail
                    googleOAuthEndpoint
                    googleAccessTokenEndpoint
                    clientInfo
                    (googleGetUserInfo clientInfo)

googleOAuthEndpoint :: OAuthEndpoint
googleOAuthEndpoint = OAuthEndpoint . fromJust . parseURI
                    $ "https://accounts.google.com/o/oauth2/auth"

googleAccessTokenEndpoint :: AccessTokenEndpoint
googleAccessTokenEndpoint = AccessTokenEndpoint . fromJust . parseURI $ "https://www.googleapis.com/oauth2/v3/token"

-- | The scope parameter for the users email address
googleScopeEmail :: QueryParams
googleScopeEmail = [("scope", "https://www.googleapis.com/auth/userinfo.email")]

googleGetUserInfo :: AuthClientInfo -> AccessToken -> App (Either Text UserId)
googleGetUserInfo clientInfo token = do
  (tokenInfo', userInfo') <- 
    withHttpManager (\m -> concurrently (validateToken m token)
                                        (getUserInfo m token))
  
  let ourClientId = decodeUtf8 $ authClientInfoId clientInfo
  tokenInfo <- failWith (const err404 {errBody =  "Error getting token info"}) tokenInfo'
  _userInfo  <- failWith (const err404 {errBody =  "Error getting user info"}) userInfo'
  when (audience tokenInfo /= ourClientId) $
    throwError err404 {errBody = "Client id didn't match"}
  findOrCreateUser _userInfo token

validateToken :: Manager -> AccessToken -> IO (OAuth2Result GoogleToken)
validateToken manager token = parseResponseJSON <$> authGetBS' manager token uri
  where uri = "https://www.googleapis.com/oauth2/v2/tokeninfo"

getUserInfo :: Manager -> AccessToken -> IO (OAuth2Result GoogleUser)
getUserInfo manager token =
  authGetJSON manager token "https://www.googleapis.com/oauth2/v2/userinfo"

failWith :: MonadError e m => (e' -> e) -> Either e' a -> m a
failWith f = \case
  Left e  -> throwError (f e)
  Right x -> pure x

findOrCreateUser :: GoogleUser -> AccessToken -> App (Either Text UserId)
findOrCreateUser user token = do
  let textId = id user
      textEmail = email user
  runHerculesQueryWithConnection (userEmailQuery textEmail) >>= \case
    []  -> createUser user token
    [u] -> case (userGoogleId u) of 
              Just "" -> updateUser u user token
              Nothing -> updateUser u user token
              _       -> pure $ Right (UserId (userId (u :: User)))
    _   -> pure $ Left "Multiple users with the same id in database!"

createUser :: GoogleUser -> AccessToken -> App (Either Text UserId)
createUser GoogleUser{..} token = do
  encryptedToken <- encrypt (accessToken token)
  let user = User () name email "" "" id encryptedToken
  withHerculesConnection (\c -> insertUser c user) >>= \case
    Nothing -> pure $ Left "Error inserting user"
    Just i -> do
      logInfo (LogString ("Added user " <> name <> " to database"))
      pure $ Right i

findUser :: UserId -> App (Maybe User) 
findUser userId = runHerculesQueryWithConnectionSingular $ userIdQuery userId 

updateUser :: User -> GoogleUser -> AccessToken -> App ( Either Text UserId)
updateUser userUpdated GoogleUser{..} token = do 
                      encryptedToken <- encrypt (accessToken token)
                      _ <- runHerculesUpdateWithConnection
                            userTable  
                            (\_ -> constantUser $ User (userId userUpdated) (userName userUpdated )(userEmail userUpdated) (userGithubId userUpdated) (userGithubToken userUpdated) (Just id) (Just encryptedToken))
                            (\oldUser -> userId oldUser .== (constant $ userId userUpdated) )
                      return $ Right $ UserId (userId userUpdated)

constantUser :: User -> UserWriteColumns 
constantUser User{..} = User  { userId          = Just $ constant userId 
                              , userName        = constant $ toMaybe userName 
                              , userEmail       = constant $ toMaybe userEmail 
                              , userGithubId    = constant $ toMaybe userGithubId 
                              , userGithubToken = constant $ toMaybe userGithubToken 
                              , userGoogleId    = constant $ toMaybe userGoogleId 
                              , userGoogleToken = constant $ toMaybe userGoogleToken 
                              } 