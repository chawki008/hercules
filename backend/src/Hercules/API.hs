{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Hercules.API
  ( API
  , QueryAPI
  , Unprotected
  , Protected
  , UserId(..)
  ) where

import Data.Text
import Servant
import Servant.Auth.Server
import Servant.HTML.Blaze
import Servant.Swagger.UI
import Text.Blaze.Html5

import Hercules.Database.Extra       (Project, ProjectWithJobsetsWithStatus, JobsetevalWithStatus, JobsetevalWithBuilds, JobsetWithInputs, QueueSummary)
import Hercules.Database.Hercules
import Hercules.OAuth.Authenticators (AuthenticatorName)
import Hercules.OAuth.Types          (AuthClientState, AuthCode, AuthError,
                                      AuthStatePacked, FrontendURL)
import Hercules.OAuth.User

type Unprotected =
      "projectNames" :> Get '[JSON] [Text]
 :<|> "projects" :> Get '[JSON] [Project]
 :<|> "projects" :> Capture "projectName" Text :> Get '[JSON] (Maybe Project)
 :<|> "projectsWithJobsets" :> Get '[JSON] [ProjectWithJobsetsWithStatus] 
 :<|> "projectsWithJobsets" :> Capture "projectId" Text :> Get '[JSON] (Maybe ProjectWithJobsetsWithStatus)
 :<|> "projects" :> Capture "projectName" Text :> Capture "jobsetName" Text :> "jobsetevals" :> Get '[JSON] [JobsetevalWithStatus]
 :<|> "projects" :> Capture "projectName" Text :> Capture "jobsetName" Text :> "jobs" :> Get '[JSON] [JobsetevalWithBuilds]
 :<|> "projects" :> ReqBody '[JSON] Project :> Post '[JSON] Text
--  :<|> "projects" :> Capture "projectId" Text :> ReqBody '[JSON] Jobset :> Post '[JSON] Text
 :<|> "projects" :> Capture "projectId" Text :> ReqBody '[JSON] JobsetWithInputs :> Post '[JSON] Text
 :<|> "queue_summary" :> Get '[JSON] QueueSummary

type Protected = "protected" :> Get '[JSON] Text

type QueryAPI = Unprotected
      :<|> Auth '[JWT] UserId :> Protected

-- | A bunch of pages used for debugging and examples
type Pages = "login" :> Get '[HTML] Html
        :<|> "login" :> Capture "authType" AuthenticatorName
                     :> QueryParam "state" AuthClientState
                     :> QueryParam "frontendURL" FrontendURL
                     :> Get '[HTML] Html
        :<|> "auth-callback" :> Capture "authType" AuthenticatorName
                             :> QueryParam "code" AuthCode
                             :> QueryParam "error" AuthError
                             :> QueryParam "state" AuthStatePacked
                             :> Get '[HTML] Html
        :<|> "logged-in" :> QueryParam "jwt" Text
                         :> Get '[HTML] Html
        :<|> "repos" :> Auth '[JWT] UserId :> Get '[HTML] Html
        :<|> "hook"  :> ReqBody '[JSON] BitbucketPR :> Post '[JSON] Text

type API = (QueryAPI
      :<|> Pages
      -- TODO: Waiting for Servant to gain Redirect combinators,
      -- The return type is wrong, this endpoint always redirects
      -- See https://github.com/haskell-servant/servant/issues/117
      :<|> Get '[HTML] Html)
      :<|> SwaggerSchemaUI "docs" "swagger.json"
