{-# LANGUAGE Arrows          #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

{-|
A module to handle the different queries we might want to make to Hercules's
database
-}
module Hercules.Query.Hercules
  ( userIdQuery
  , userGitHubIdQuery
  , insertUser
  , userGoogleIdQuery
  , userEmailQuery
  ) where

import Control.Arrow              (returnA)
import Data.ByteString         as BS
import Data.Text               as Text
import Database.PostgreSQL.Simple (Connection)
import Opaleye.Extra

import Hercules.Database.Hercules
import Hercules.OAuth.User

-- | A query to get a user from their id
userIdQuery :: UserId -> Query UserReadColumns
userIdQuery (UserId uid) = proc () -> do
  user@User{..} <- queryTable userTable -< ()
  restrict -< pgInt8 uid .== userId
  returnA -< user

-- | A query to get a user by their github id
userGitHubIdQuery :: Text -> Query UserReadColumns
userGitHubIdQuery githubId = proc () -> do
  user@User{..} <- queryTable userTable -< ()
  restrict -< pgStrictText githubId `eqNullable` userGithubId
  returnA -< user

-- | A query to get a user by their google id
userGoogleIdQuery :: Text -> Query UserReadColumns
userGoogleIdQuery googleId = proc () -> do
  user@User{..} <- queryTable userTable -< ()
  restrict -< pgStrictText googleId `eqNullable` userGoogleId
  returnA -< user

-- | A query to get a user by their email
userEmailQuery :: Text -> Query UserReadColumns
userEmailQuery email = proc () -> do
  user@User{..} <- queryTable userTable -< ()
  restrict -< pgStrictText email `eqNullable` userEmail
  returnA -< user

insertUser :: Connection -> User' a Text Text Text ByteString Text ByteString -> IO (Maybe UserId)
insertUser c User { userName
                  , userEmail
                  , userGithubId
                  , userGithubToken
                  , userGoogleId
                  , userGoogleToken 
                  } =
  let user =
        User
          Nothing
          (Just (toNullable (pgStrictText userName)))
          (Just (toNullable (pgStrictText userEmail)))
          (if Text.null userGithubId then Nothing else (Just (toNullable (pgStrictText userGithubId)) ))
          (if BS.null userGithubToken then Nothing else (Just (toNullable (pgStrictByteString userGithubToken)) ))
          (if Text.null userGoogleId then Nothing else (Just (toNullable (pgStrictText userGoogleId)) ))
          (if BS.null userGoogleToken then Nothing else (Just (toNullable (pgStrictByteString userGoogleToken)) ))
          
  in runInsertManyReturning c userTable [user] userId >>=
     \case
       [i] -> pure $ Just (UserId i)
       _ -> pure Nothing
