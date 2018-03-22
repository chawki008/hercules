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
                                      ProjectWithJobsets (..), JobsetStatus (..), Jobset, Jobseteval,
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
                      :<|> getJobsetEvals
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
 
  
makeProjectWithJobsets :: Project -> [JobsetNullable] -> ProjectWithJobsets
makeProjectWithJobsets p jms =
  let js = catMaybes (fromNullableJobset <$> jms)
  in ProjectWithJobsets p js

groupSortOn :: Ord k => (a -> k) -> [(a, v)] -> [(a, NE.NonEmpty v)]
groupSortOn f = fmap (\x -> (fst $ NE.head x, fmap snd x))
          . NE.groupWith (f . fst)
          . sortOn (f . fst)

getJobsetEvals :: Text -> Text -> App [Jobseteval]
getJobsetEvals projectName jobsetName = runHydraQueryWithConnection (jobsetevalsQuery projectName jobsetName)


getProjectWithJobsetsWithStatus :: Text -> App (Maybe ProjectWithJobsetsWithStatus)
getProjectWithJobsetsWithStatus projectname = fmap headMay $  fmap (uncurry makeProjectWithJobsetsWithStatus . second toList)
  . groupSortOnWihtStatus projectName
  <$> (getRidOfNullable <$>) <$> (runHydraQueryWithConnection (projectsWithJobsetWithStatusQuery projectname) :: App [(Project, JobsetNullable, Maybe Int64, Maybe Int64, Maybe Int64, Maybe Int32 )])
 
makeProjectWithJobsetsWithStatus :: Project -> [Maybe JobsetWithStatus] -> ProjectWithJobsetsWithStatus
makeProjectWithJobsetsWithStatus p jms =
  let js = catMaybes jms
  in ProjectWithJobsetsWithStatus p js

groupSortOnWihtStatus :: Ord k => (Project -> k) -> [(Project, Maybe Jobset, Int64, Int64, Int64, Maybe(Int32))] -> [(Project, NE.NonEmpty (Maybe JobsetWithStatus))]
groupSortOnWihtStatus f = fmap (\x -> (fst6 $ NE.head x, fmap createJobsetWithStatus x))
          . NE.groupWith (f . fst6)
          . sortOn (f . fst6)

fst6 :: (a,b,c,d,e,f) -> a
fst6 (a,_,_,_,_,_) = a

createJobsetWithStatus :: (Project, Maybe Jobset, Int64, Int64, Int64, Maybe(Int32)) -> Maybe JobsetWithStatus
createJobsetWithStatus (_, Just jobset, queued, failed, succeeded, lastevaluatedAt) = Just JobsetWithStatus { jobset = jobset
                                                                                 , jobsetStatus = jobsetstatus
                                                                                 }
                                                                                 where jobsetstatus = JobsetStatus  { succeeded = succeeded
                                                                                                                    , failed = failed
                                                                                                                    , queued = queued 
                                                                                                                    , lastevaluatedAt = lastevaluatedAt
                                                                                                                    }
createJobsetWithStatus (_, Nothing, queued, failed, succeeded, lastevaluatedAt) = Nothing                                                                         

getProjectsWithJobsetsWithStatus :: App [ProjectWithJobsetsWithStatus]
getProjectsWithJobsetsWithStatus =
  fmap (uncurry makeProjectWithJobsetsWithStatus . second toList)
  . groupSortOnWihtStatus projectName
  <$> (fmap getRidOfNullable ) 
  <$> (runHydraQueryWithConnection (projectsWithJobsetWithStatusQuery (pack "")):: App [(Project, JobsetNullable, Maybe Int64, Maybe Int64, Maybe Int64, Maybe Int32 )])

getRidOfNullable :: (Project, JobsetNullable, Maybe Int64, Maybe Int64, Maybe Int64, Maybe Int32)  -> (Project, Maybe Jobset, Int64, Int64, Int64, Maybe Int32)
getRidOfNullable (p, j, queued, failed, succeeded, lastEvaluatedAt)  = (p, js, fromMaybe 0 queued, fromMaybe 0 failed, fromMaybe 0 succeeded, lastEvaluatedAt)
  where js = fromNullableJobset j