{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Hercules.Lib
  ( startApp
  , swaggerDoc
  , getAppForTest
  ) where

import Control.Monad                        (join)
import Control.Monad.Log
import Data.Monoid                          ((<>))
import Data.Text
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Servant
import Servant.Auth.Server                  (AuthResult (..),
                                             defaultCookieSettings)
import Servant.Mandatory
import Servant.Redirect
import Servant.Swagger.UI

import qualified Data.Text.IO       as T

import Hercules.API
import Hercules.Config

import Hercules.OAuth
import Hercules.OAuth.Authenticators
import Hercules.ServerEnv
import Hercules.Static
import Hercules.Swagger
import Hercules.Controllers.Jobset          (addJobsetWithInputs)
import Hercules.Controllers.Project         (getProjectNames, getProjects, getProject, getProjectsWithJobsetsWithStatus, getProjectWithJobsetsWithStatus, addProject)
import Hercules.Controllers.Jobseteval      (getJobsetEvals, getJobsetevalsWithBuilds)
import Data.Yaml                            (decodeFileEither, prettyPrintParseException)
import Hercules.Controllers.QueueSummary    (getQueueSummary)

startApp :: Config -> IO ()
startApp config = do
  let authenticators = configAuthenticatorList config
      port = configPort config
      logging = loggingMiddleware config
  newEnv config authenticators >>= \case
    Nothing -> pure ()
    Just env -> do
      T.putStrLn $ "Serving on http://" <> configHostname config
                   <> ":" <> (pack . show $ port)
      run port . logging =<< app env

loggingMiddleware :: Config -> Middleware
loggingMiddleware config = case configAccessLogLevel config of
  Disabled    -> id
  Enabled     -> logStdout
  Development -> logStdoutDev

getAppForTest :: FilePath -> IO Application 
getAppForTest yaml = 
  let 
    authenticators = configAuthenticatorList 
  in 
    decodeFileEither yaml >>= \case
    Left err -> error (prettyPrintParseException err)
    Right config ->
      newEnv config (authenticators config)>>= \case
        Nothing -> error "Can't create env"
        Just env -> app env

  
app :: Env -> IO Application
app env = do
  let api = Proxy :: Proxy API
      authConfig = defaultCookieSettings :. envJWTSettings env :. EmptyContext
  pure $ serveWithContext api authConfig (server env)

server :: Env -> Server API
server env = enter (Nat (runApp env)) api :<|> serveSwagger
  where api = queryApi
              :<|> pages
              :<|> root
        pages = welcomePage
                :<|> (mandatory1 .: loginPage)
                :<|> (mandatory1 .∵ authCallback)
                :<|> loggedInPage
                :<|> (join . withAuthenticated userInfoPage)
        queryApi = unprotected :<|> protected
        unprotected = getProjectNames
                      :<|> getProjects
                      :<|> getProject
                      :<|> getProjectsWithJobsetsWithStatus
                      :<|> getProjectWithJobsetsWithStatus
                      :<|> getJobsetEvals
                      :<|> getJobsetevalsWithBuilds
                      :<|> addProject 
                      -- :<|> addJobset 
                      :<|> addJobsetWithInputs 
                      :<|> getQueueSummary 
        protected = getUser

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

(.∵) :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
(.∵) = (.) . (.) . (.)


root :: App a
root = redirectBS "/docs/"

serveSwagger :: Server (SwaggerSchemaUI "docs" "swagger.json")
serveSwagger = swaggerSchemaUIServer swaggerDoc

getUser :: AuthResult UserId -> App Text
getUser = withAuthenticated (pack . show)

withAuthenticated :: (a -> b) -> AuthResult a -> App b
withAuthenticated f = \case
  (Authenticated x) -> pure (f x)
  _                 -> do
    logNotice "Failed user authentication attempt"
    throwError err401



                                  


