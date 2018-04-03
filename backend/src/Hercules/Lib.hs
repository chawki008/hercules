{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Hercules.Lib
  ( startApp
  , swaggerDoc
  ) where

import Control.Monad                        (join)
import Control.Monad.Log
import Data.Monoid                          ((<>))
import Data.Text
import Data.Int                              (Int32, Int64)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Safe                                 (headMay)
import Servant
import Servant.Auth.Server                  (AuthResult (..),
                                             defaultCookieSettings)
import Servant.Mandatory
import Servant.Redirect
import Data.Bifunctor                       (second)
import Data.Foldable                        (toList)
import Servant.Swagger.UI

import qualified Data.Text.IO       as T
import qualified Data.List.NonEmpty as NE
import Data.Maybe                           (catMaybes)

import Hercules.API
import Hercules.Config
import Hercules.Database.Extra       (JobsetNullable, Project, JobsetevalinputNullable
                                     , Jobseteval, Job, Build, JobsetevalWithBuilds(..), BuildNullable
                                     , ProjectWithJobsetsWithStatus (..), JobsetevalWithStatus(..)
                                     , jobsetevalId, projectName, fromNullableBuild)
import Hercules.OAuth
import Hercules.OAuth.Authenticators
import Hercules.Query.Hydra
import Hercules.ServerEnv
import Hercules.Static
import Hercules.Swagger
import Hercules.Helpers
import Data.List                            (sortOn)

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

getJobsetevalsWithBuilds :: Text -> Text -> App [JobsetevalWithBuilds]
getJobsetevalsWithBuilds projectNameArg jobset = Prelude.take 10 <$>  (makeJobsetevalsWithBuilds . groupSortJobsetevalsWithBuilds) 
                                                <$> (runHydraQueryWithConnection $ jobsetevalsWithBuildsQuery projectNameArg jobset) 

makeJobsetevalsWithBuilds ::  [(Jobseteval, NE.NonEmpty (Maybe Build))] -> [JobsetevalWithBuilds]
makeJobsetevalsWithBuilds  = fmap makeJobsetevalWithBuilds

makeJobsetevalWithBuilds :: (Jobseteval, NE.NonEmpty (Maybe Build)) -> JobsetevalWithBuilds
makeJobsetevalWithBuilds jobsetevalsWBuilds = JobsetevalWithBuilds { jobsetevalWithBuildsEval = fst jobsetevalsWBuilds
                                                                   , builds = Prelude.take 250 $ (catMaybes . toList .snd) jobsetevalsWBuilds 
                                                                   }

groupSortJobsetevalsWithBuilds ::  [(Jobseteval, BuildNullable)] -> [(Jobseteval, NE.NonEmpty (Maybe Build))]
groupSortJobsetevalsWithBuilds  = fmap (\x -> (fst $ NE.head x, fmap createBuilds x))
          . NE.groupWith (jobsetevalId . fst)
          . sortOn (jobsetevalId . fst)
    where 
      createBuilds = fromNullableBuild . snd 

getProjectNames :: App [Text]
getProjectNames = runHydraQueryWithConnection projectNameQuery

getProject :: Text -> App (Maybe Project)
getProject name = headMay <$> runHydraQueryWithConnection (projectQuery name)
 
getProjects :: App [Project]
getProjects = runHydraQueryWithConnection projectsQuery
 
getJobsetEvals :: Text -> Text -> App [JobsetevalWithStatus]
getJobsetEvals projectNameArg jobsetNameArg = updateChangedInputs <$> (fmap makeJobsetevalWithStatus . groupSortOnJobsetevals jobsetevalId
 <$> (runHydraQueryWithConnection (jobsetevalsWithStatusQuery projectNameArg jobsetNameArg):: App [(Jobseteval, Maybe Int64, Maybe Int64, JobsetevalinputNullable)]))

getProjectWithJobsetsWithStatus :: Text -> App (Maybe ProjectWithJobsetsWithStatus)
getProjectWithJobsetsWithStatus projectNameArg = fmap headMay $ fmap (uncurry makeProjectWithJobsetsWithStatus . second toList)
  . groupSortOnProjectWihtStatus projectName
  <$> (getRidOfNullable <$>) <$> (runHydraQueryWithConnection (projectsWithJobsetWithStatusQuery projectNameArg) :: App [(Project, JobsetNullable, Maybe Int64, Maybe Int64, Maybe Int64, Maybe Int32 )])

getProjectsWithJobsetsWithStatus :: App [ProjectWithJobsetsWithStatus]
getProjectsWithJobsetsWithStatus =
  fmap (uncurry makeProjectWithJobsetsWithStatus . second toList)
  . groupSortOnProjectWihtStatus projectName
  <$> (fmap getRidOfNullable ) 
  <$> (runHydraQueryWithConnection (projectsWithJobsetWithStatusQuery (pack "")):: App [(Project, JobsetNullable, Maybe Int64, Maybe Int64, Maybe Int64, Maybe Int32 )])

getJobsetJobs :: Text -> Text -> App [Job]
getJobsetJobs projectNameArg jobsetNameArg = runHydraQueryWithConnection (jobsetjobsQuery projectNameArg jobsetNameArg)
