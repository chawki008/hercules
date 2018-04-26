{-# LANGUAGE Arrows          #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}


{-|
A module to handle the different subqueries we might want to make to Hydra's
database concerning projects with their jobsets
-}
module Hercules.Query.Jobsets
  ( projectsWithJobsetWithStatusQuery
  , projectsWithJobsetWithStatus
  , projectQuery
  , projectsQuery
  , jobsetsQuery
  , projectNameQuery
  , queueLengthByJobsetQuery
  , projectByRepoQuery
  ) where

import Control.Arrow (returnA)
import Data.Text
import Opaleye
import Data.Profunctor.Product (p3)
import Hercules.Database.Hydra

-- query to get a list of all projects wiht their jobsets with their status (returs Query (prject, jobset, nrQueued, nrFailed, nrSucceeded, lastEvaluatedAt))
projectsWithJobsetWithStatusQuery :: Text -> Query (ProjectReadColumns, JobsetNullableColumns,
                                                     Column (Nullable PGInt8), Column (Nullable PGInt8), Column (Nullable PGInt8), Column (Nullable PGInt4))
projectsWithJobsetWithStatusQuery projectNameArg = proc() -> do
    ((project, jobset, nrQueued, nrFailed, nrSucceeded), (_, _, lastEvaluatedAt)) <- (projectsWithJobsetWithStatus projectNameArg) -< ()
    returnA -< (project, jobset, nrQueued, nrFailed, nrSucceeded, lastEvaluatedAt)  

projectsWithJobsetWithStatus :: Text -> Query ((ProjectReadColumns, JobsetNullableColumns, Column (Nullable PGInt8), Column (Nullable PGInt8), Column (Nullable PGInt8)),
                                                 (Column (Nullable PGText), Column (Nullable PGText), Column (Nullable PGInt4)))
projectsWithJobsetWithStatus projectNameArg = leftJoin (projectsWithJobsetNrSucceededQuery projectNameArg) lastEvaluatedatByJobsetQuery eqName 
  where 
    eqName ((Project{..}, Jobset{..}, _, _, _), ( lastEvalAtJobset, lastEvalAtProject, _ )) = (projectName .==  lastEvalAtProject) .&& ((Opaleye.fromNullable  (pgStrictText $ pack "") jobsetName) .==  lastEvalAtJobset) 

lastEvaluatedatByJobsetQuery :: Query (Column PGText, Column PGText, Column PGInt4 )
lastEvaluatedatByJobsetQuery  = aggregate (p3(Opaleye.groupBy, Opaleye.groupBy, Opaleye.max))  (proc () -> do 
                            Jobseteval{..} <- queryTable jobsetevalTable -< () 
                            returnA -< (jobsetevalJobset, jobsetevalProject, jobsetevalTimestamp)
                            )   

projectsWithJobsetNrSucceededQuery :: Text -> Query (ProjectReadColumns, JobsetNullableColumns,
                                                     Column (Nullable PGInt8), Column (Nullable PGInt8), Column (Nullable PGInt8))
projectsWithJobsetNrSucceededQuery projectNameArg = proc() -> do
    ((project, jobset, nrQueued, nrFailed), (_, _, nrSucceeded)) <- (projectsWithJobsetNrSucceeded projectNameArg) -< ()
    returnA -< (project, jobset, nrQueued, nrFailed, nrSucceeded)  

projectsWithJobsetNrSucceeded :: Text -> Query ((ProjectReadColumns, JobsetNullableColumns, Column (Nullable PGInt8), Column (Nullable PGInt8)),
                                                 (Column (Nullable PGText), Column (Nullable PGText), Column (Nullable PGInt8)))
projectsWithJobsetNrSucceeded projectNameArg = leftJoin (projectsWithJobsetWithQueueWithFailedQuery projectNameArg) nrSucceededByJobsetQuery eqName 
    where 
      eqName ((Project{..}, Jobset{..}, _, _), ( succJobset, succProject, _ )) = (projectName .==  succProject)  .&& ( (Opaleye.fromNullable  (pgStrictText $ pack "") jobsetName) .==  succJobset) 

nrSucceededByJobsetQuery :: Query (Column PGText, Column PGText, Column PGInt8)
nrSucceededByJobsetQuery = aggregate (p3(Opaleye.groupBy, Opaleye.groupBy, countStar))  (proc () -> do 
  Build{..} <- queryTable buildTable -< () 
  restrict -< ((buildFinished .== pgInt4 1) .&& ( (fromNull (pgInt4 0) buildIscurrent) .== pgInt4 1) .&& ( (fromNull (pgInt4 1) buildBuildstatus) .== pgInt4 0) )
  returnA -< (buildJobset, buildProject, buildId)
  )
  where 
    fromNull = Opaleye.fromNullable 
                              
projectsWithJobsetWithQueueWithFailedQuery :: Text -> Query (ProjectReadColumns, JobsetNullableColumns,
                                                             Column (Nullable PGInt8), Column (Nullable PGInt8))
projectsWithJobsetWithQueueWithFailedQuery projectNameArg = proc() -> do
    ((project, jobset, nrQueued), (_, _, nrFailed)) <- (projectsWithJobsetWithQueueWithFailed projectNameArg)-< ()
    returnA -< (project, jobset, nrQueued, nrFailed)  

projectsWithJobsetWithQueueWithFailed :: Text -> Query ((ProjectReadColumns, JobsetNullableColumns, Column (Nullable PGInt8)),
                                                    (Column (Nullable PGText), Column (Nullable PGText), Column (Nullable PGInt8) ) )
projectsWithJobsetWithQueueWithFailed projectNameArg = leftJoin (projectsWithJobsetWithQueueQuery projectNameArg) nrFailedByJobsetQuery eqName 
    where 
      eqName ((Project{..}, Jobset{..}, _), ( failJobset, failProject, _ )) = (projectName .==  failProject) .&&
                                                   ( (Opaleye.fromNullable  (pgStrictText $ pack "") jobsetName) .==  failJobset) 
nrFailedByJobsetQuery :: Query (Column PGText, Column PGText, Column PGInt8)
nrFailedByJobsetQuery = aggregate (p3(Opaleye.groupBy, Opaleye.groupBy, countStar))  (proc () -> do 
  Build{..} <- queryTable buildTable -< () 
  restrict -< ((buildFinished .== pgInt4 1) .&& ( (Opaleye.fromNullable (pgInt4 0) buildIscurrent) .== pgInt4 1) .&& ((Opaleye.fromNullable (pgInt4 1) buildBuildstatus) ./= pgInt4 0))
  returnA -< (buildJobset, buildProject, buildId)
  )    

projectsWithJobsetWithQueueQuery :: Text -> Query (ProjectReadColumns, JobsetNullableColumns , Column (Nullable PGInt8) )
projectsWithJobsetWithQueueQuery projectNameArg = proc() -> do
    ((project, jobset), (_, _, queued)) <- (projectsWithJobsetWithQueue projectNameArg) -< ()
    returnA -< (project, jobset, queued)  
      
projectsWithJobsetWithQueue :: Text -> Query ((ProjectReadColumns, JobsetNullableColumns),
                                               (Column (Nullable PGText), Column (Nullable PGText), Column (Nullable PGInt8)))
projectsWithJobsetWithQueue projectNameArg = leftJoin (projectWithJobsetsQuery projectNameArg) queueLengthByJobsetQuery eqName 
    where 
      eqName ((Project{..}, Jobset{..}), ( schJobset, schProject, _ )) = (projectName .==  schProject) .&&
                                                                     ( (Opaleye.fromNullable  (pgStrictText $ pack "") jobsetName) .==  schJobset) 

queueLengthByJobsetQuery :: Query (Column PGText, Column PGText, Column PGInt8)
queueLengthByJobsetQuery = aggregate (p3(Opaleye.groupBy, Opaleye.groupBy, countStar))  (proc () -> do 
                            Build{..} <- queryTable buildTable -< () 
                            restrict -< ((buildFinished .== pgInt4 0) .&& ( (Opaleye.fromNullable (pgInt4 0) buildIscurrent) .== pgInt4 1) )
                            returnA -< (buildJobset, buildProject, buildId)
                            )    

projectWithJobsetsQuery :: Text -> Query (ProjectReadColumns, JobsetNullableColumns)
projectWithJobsetsQuery projectNameArg 
            | Data.Text.null projectNameArg = leftJoin projectsQuery jobsetsQuery eqName
            | otherwise = leftJoin (projectQuery projectNameArg) jobsetsQuery eqName
  where
    eqName (Project{..}, Jobset{..}) = projectName .== jobsetProject

-- | A query to get a list of all the projects
projectsQuery :: Query ProjectReadColumns
projectsQuery = queryTable projectTable

-- | A query to get all the projects with the specified name (There should be
-- only one)
projectQuery :: Text -> Query ProjectReadColumns
projectQuery name = proc () -> do
  project@Project{..} <- queryTable projectTable -< ()
  restrict -< projectName .== pgStrictText name
  returnA -< project

-- | A query to get a list of all the jobsets
jobsetsQuery :: Query JobsetReadColumns
jobsetsQuery = queryTable jobsetTable 

-- | A query to get a list of all the project names
projectNameQuery :: Query (Column PGText)
projectNameQuery = proc () -> do
  Project{..} <- queryTable projectTable -< ()
  returnA -< projectName
  
projectByRepoQuery :: Text -> Query ProjectReadColumns
projectByRepoQuery repo = proc () -> do
  project@Project{..} <- queryTable projectTable -< ()
  restrict -< (Opaleye.fromNullable (pgStrictText "") projectRepo) .== pgStrictText repo
  returnA -< project