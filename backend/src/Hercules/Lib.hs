{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Hercules.Lib
  ( startApp
  , swaggerDoc
  ) where

import Control.Monad                        (join)
import Control.Monad.Log
import Data.Bifunctor                       (second)
import Data.Foldable                        (toList)
import Data.List                            (sortOn)
import Data.Maybe                           (catMaybes, fromMaybe)
import Data.Monoid                          ((<>))
import Data.Swagger
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
import Servant.Swagger
import Servant.Swagger.UI

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.IO       as T

import Hercules.API
import Hercules.Config
import Hercules.Database.Extra       (JobsetNullable, Project,
                                      ProjectWithJobsets (..), JobsetStatus (..), Jobset,
                                      JobsetWithStatus (..), ProjectWithJobsetsWithStatus (..),
                                      jobsetName, jobsetProject,
                                      fromNullableJobset, projectName)
import Hercules.OAuth
import Hercules.OAuth.Authenticators
import Hercules.Query.Hydra
import Hercules.ServerEnv
import Hercules.Static
import Hercules.Swagger

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

getProjectNames :: App [Text]
getProjectNames = runHydraQueryWithConnection projectNameQuery

getProject :: Text -> App (Maybe Project)
getProject name = headMay <$> runHydraQueryWithConnection (projectQuery name)

getProjects :: App [Project]
getProjects = runHydraQueryWithConnection projectsQuery

getProjectsWithJobsets :: App [ProjectWithJobsets]
getProjectsWithJobsets =
  fmap (uncurry makeProjectWithJobsets . second toList)
  . groupSortOn projectName
  <$> (runHydraQueryWithConnection projectsWithJobsetsQuery :: App [(Project, JobsetNullable)])
  where
    makeProjectWithJobsets :: Project -> [JobsetNullable] -> ProjectWithJobsets
    makeProjectWithJobsets p jms =
      let js = catMaybes (fromNullableJobset <$> jms)
      in ProjectWithJobsets p js

groupSortOn :: Ord k => (a -> k) -> [(a, v)] -> [(a, NE.NonEmpty v)]
groupSortOn f = fmap (\x -> (fst $ NE.head x, fmap snd x))
          . NE.groupWith (f . fst)
          . sortOn (f . fst)


getProjectsWithJobsetsWithStatus :: App [ProjectWithJobsetsWithStatus]
getProjectsWithJobsetsWithStatus = getProjectsWithJobsets >>= addStatus 

addStatus :: [ProjectWithJobsets] -> App [ProjectWithJobsetsWithStatus]
addStatus projectsWithJobsets = sequence (addStatusByProject <$>  projectsWithJobsets )

addStatusByProject :: ProjectWithJobsets -> App (ProjectWithJobsetsWithStatus)
addStatusByProject projectWithJobsets = do 
    jobsets <- sequence $ addStatusByJobset <$>  projectWithJobsetsJobsets projectWithJobsets
    return (ProjectWithJobsetsWithStatus  { project =  projectWithJobsetsProject projectWithJobsets
                                          , jobsets = jobsets   
                                          }
           )

addStatusByJobset :: Jobset -> App (JobsetWithStatus)
addStatusByJobset jobset = do 
    jobsetStatus <- getJobsetStatus (jobsetName jobset) (jobsetProject jobset)  
    return (JobsetWithStatus { jobset = jobset
                             , jobsetStatus = jobsetStatus
                             }
           )

getJobsetStatus :: Text -> Text -> App (JobsetStatus)
getJobsetStatus jobsetName jobsetProject= do
    (succeed, builds, lastevaluatedAt) <- getSucceededFailedLastEvaluated jobsetName jobsetProject
    queued <- getQueued jobsetName jobsetProject
    return (JobsetStatus  { succeed = succeed
                          , failed =  (-) <$> builds <*> Just ( fromMaybe 0 succeed) 
                          , queued = queued
                          , lastevaluatedAt = lastevaluatedAt
                          })

getSucceededFailedLastEvaluated :: Text -> Text -> App((Maybe Int32, Maybe Int32, Int32))
getSucceededFailedLastEvaluated jobsetName jobsetProject = (fromMaybe (Just 0, Just 0, 0)) <$> (headMay <$> runHydraQueryWithConnection (jobsetSucceedFailedLastEvaluatedQuery  jobsetName jobsetProject))

getQueued :: Text -> Text -> App(Maybe Int64)
getQueued jobsetName jobsetProject = headMay <$> runHydraQueryWithConnection (jobsetQueueLengthQuery jobsetName jobsetProject)  ;

getProjectWithJobsetsWithStatus :: Text -> App (Maybe ProjectWithJobsetsWithStatus)
getProjectWithJobsetsWithStatus projectId = (getProjectWithJobsets projectId) >>=  sequence.(fmap addStatusByProject)  

getProjectWithJobsets :: Text -> App (Maybe ProjectWithJobsets)
getProjectWithJobsets projectId =  headMay <$> fmap (uncurry makeProjectWithJobsets . second toList)
  . groupSortOn projectName
  <$> (runHydraQueryWithConnection (projectWithJobsetsQuery projectId) :: App [(Project, JobsetNullable)]) 