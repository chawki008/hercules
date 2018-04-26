{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Hercules.Controllers.Project (getProjectNames
                                    , getProjects
                                    , getProject
                                    , getProjectsWithJobsetsWithStatus
                                    , getProjectWithJobsetsWithStatus
                                    , addProject) where 

import Hercules.Helpers.Helpers 
import Servant
import Hercules.Query.Hydra
import Data.List                            (sortOn)
import Data.Int                      (Int32, Int64)
import Opaleye
import Data.Text
import Data.Maybe                            (catMaybes, fromMaybe)
import Hercules.ServerEnv
import Safe                                 (headMay)
import Data.Bifunctor                       (second)
import Data.Foldable                        (toList)
import qualified Data.List.NonEmpty as NE


import Hercules.Database.Extra       ( JobsetNullable, Project, Jobset, Project'(..), JobsetWithStatus(..)
                                     , ProjectWithJobsetsWithStatus (..), JobsetStatus(..), ProjectWriteColumns
                                     , projectTable, fromNullableJobset)

addProject :: Project -> App (Text) 
addProject project =  do 
            mProject <- getProject $ projectName project
            case mProject of 
                Just _ -> throwError $ err409 { errBody = "project already exists" }
                Nothing -> runHydraUpdateWithConnection projectTable [(constantProject project)]
            return ("Project added successfully")           

constantProject :: Project -> ProjectWriteColumns 
constantProject project = Project { projectName = constant $ projectName project
                                  , projectDisplayname = constant $ projectDisplayname project
                                  , projectDescription = constant $ toMaybe $ projectDescription project
                                  , projectEnabled = constant $ projectEnabled project
                                  , projectHidden = constant $ projectHidden project
                                  , projectOwner = constant $ projectOwner project
                                  , projectHomepage = constant $ toMaybe $ projectHomepage project
                                  , projectRepo = constant $ toMaybe $ projectRepo project
                                  }


getProjectNames :: App [Text]
getProjectNames = runHydraQueryWithConnection projectNameQuery

getProject :: Text -> App (Maybe Project)
getProject name = headMay <$> runHydraQueryWithConnection (projectQuery name)
 
getProjects :: App [Project]
getProjects = runHydraQueryWithConnection projectsQuery

getProjectWithJobsetsWithStatus :: Text -> App (Maybe ProjectWithJobsetsWithStatus)
getProjectWithJobsetsWithStatus projectNameArg = fmap headMay $ fmap (uncurry makeProjectWithJobsetsWithStatus . second toList)
  . groupSortOnProjectWihtStatus projectName
  <$> (getRidOfNullable <$>) <$> (runHydraQueryWithConnection (projectsWithJobsetWithStatusQuery projectNameArg) :: App [(Project, JobsetNullable, Maybe Int64, Maybe Int64, Maybe Int64, Maybe Int32 )])

getProjectsWithJobsetsWithStatus :: App [ProjectWithJobsetsWithStatus]
getProjectsWithJobsetsWithStatus =
  fmap (uncurry makeProjectWithJobsetsWithStatus . second toList)
  . groupSortOnProjectWihtStatus projectName
  <$> (fmap getRidOfNullable ) 
  <$> (runHydraQueryWithConnection (projectsWithJobsetWithStatusQuery ""):: App [(Project, JobsetNullable, Maybe Int64, Maybe Int64, Maybe Int64, Maybe Int32 )])

groupSortOnProjectWihtStatus :: Ord k => (Project -> k) -> [(Project, Maybe Jobset, Int64, Int64, Int64, Maybe(Int32))] -> [(Project, NE.NonEmpty (Maybe JobsetWithStatus))]
groupSortOnProjectWihtStatus f = fmap (\x -> (fst6 $ NE.head x, fmap createJobsetWithStatus x))
          . NE.groupWith (f . fst6)
          . sortOn (f . fst6)   

getRidOfNullable :: (Project, JobsetNullable, Maybe Int64, Maybe Int64, Maybe Int64, Maybe Int32)  -> (Project, Maybe Jobset, Int64, Int64, Int64, Maybe Int32)
getRidOfNullable (p, j, queued, failed, succeeded, lastEvaluatedAt)  = (p, js, notNull queued, notNull failed, notNull succeeded, lastEvaluatedAt)
  where js = fromNullableJobset j ;
        notNull = fromMaybe 0 ;

createJobsetWithStatus :: (Project, Maybe Jobset, Int64, Int64, Int64, Maybe(Int32)) -> Maybe JobsetWithStatus
createJobsetWithStatus (_, Just jobsetArg, queued, failed, succeeded, lastevaluatedAt) = Just JobsetWithStatus { jobset = jobsetArg
                                                                                 , jobsetStatus = jobsetstatus
                                                                                 }
                                                                                 where jobsetstatus = JobsetStatus  { jobsetNrsucceeded = succeeded
                                                                                                                    , jobsetNrfailed = failed
                                                                                                                    , jobsetNrqueued = queued 
                                                                                                                    , jobsetLastevaluatedAt = lastevaluatedAt
                                                                                                                    }
createJobsetWithStatus (_, Nothing, _, _, _, _) = Nothing    

makeProjectWithJobsetsWithStatus :: Project -> [Maybe JobsetWithStatus] -> ProjectWithJobsetsWithStatus
makeProjectWithJobsetsWithStatus p jms =
  let js = catMaybes jms
  in ProjectWithJobsetsWithStatus p js