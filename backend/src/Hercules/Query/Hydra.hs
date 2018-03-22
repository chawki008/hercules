{-# LANGUAGE Arrows          #-}
{-# LANGUAGE RecordWildCards #-}

{-|
A module to handle the different queries we might want to make to Hydra's
database
-}
module Hercules.Query.Hydra
  ( projectNameQuery
  , projectQuery
  , projectsQuery
  , jobsetevalsQuery
  , projectsWithJobsetWithStatusQuery
  ) where

import Control.Arrow (returnA)
import Data.Text
import Opaleye
import Data.Profunctor.Product (p3)
import Hercules.Database.Hydra

-- | A query to get a list of all the project names
projectNameQuery :: Query (Column PGText)
projectNameQuery = proc () -> do
  Project{..} <- queryTable projectTable -< ()
  returnA -< projectName

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
    


projectWithJobsetsQuery
  :: Text -> Query (ProjectReadColumns, JobsetNullableColumns)
projectWithJobsetsQuery projectName 
            | Data.Text.null projectName = leftJoin projectsQuery jobsetsQuery eqName
            | otherwise = leftJoin (projectQuery projectName) jobsetsQuery eqName
  where
    eqName (Project{..}, Jobset{..}) = projectName .== jobsetProject

jobsetevalsQuery :: Text -> Text -> Query JobsetevalReadColumns     
jobsetevalsQuery projectName jobsetName = orderBy (desc jobsetevalTimestamp) $ proc () -> do 
  jobseteval@Jobseteval{..} <- queryTable jobsetevalTable -< ()
  restrict -< ((jobsetevalJobset .== pgStrictText jobsetName) .&& (jobsetevalProject .== pgStrictText projectName))
  returnA -< jobseteval  

projectsWithJobsetWithQueueQuery :: Text -> Query (ProjectReadColumns, JobsetNullableColumns , Column (Nullable PGInt8) )
projectsWithJobsetWithQueueQuery projectName = proc() -> do
    ((project, jobset), (_, _, count)) <- (projectsWithJobsetWithQueue projectName) -< ()
    returnA -< (project, jobset, count)  
      
projectsWithJobsetWithQueue :: Text -> Query ((ProjectReadColumns, JobsetNullableColumns),
                                               (Column (Nullable PGText), Column (Nullable PGText), Column (Nullable PGInt8)))
projectsWithJobsetWithQueue projectName = leftJoin (projectWithJobsetsQuery projectName) queueLengthByJobsetQuery eqName 
    where 
      eqName ((Project{..}, Jobset{..}), ( schJobset, schProject, _ )) = (projectName .==  schProject) .&&
                                                                     ( (Opaleye.fromNullable  (pgStrictText $ pack "") jobsetName) .==  schJobset) 

queueLengthByJobsetQuery :: Query (Column PGText, Column PGText, Column PGInt8)
queueLengthByJobsetQuery = aggregate (p3(Opaleye.groupBy, Opaleye.groupBy, countStar))  (proc () -> do 
                            Build{..} <- queryTable buildTable -< () 
                            restrict -< ((buildFinished .== pgInt4 0) .&& ( (Opaleye.fromNullable (pgInt4 0) buildIscurrent) .== pgInt4 1) )
                            returnA -< (buildJobset, buildProject, buildId)
                            )    

projectsWithJobsetWithQueueWithFailedQuery :: Text -> Query (ProjectReadColumns, JobsetNullableColumns,
                                                             Column (Nullable PGInt8), Column (Nullable PGInt8))
projectsWithJobsetWithQueueWithFailedQuery projectName = proc() -> do
    ((project, jobset, nrQueued), (_, _, nrFailed)) <- (projectsWithJobsetWithQueueWithFailed projectName)-< ()
    returnA -< (project, jobset, nrQueued, nrFailed)  

projectsWithJobsetWithQueueWithFailed :: Text -> Query ((ProjectReadColumns, JobsetNullableColumns, Column (Nullable PGInt8)),
                                                    (Column (Nullable PGText), Column (Nullable PGText), Column (Nullable PGInt8) ) )
projectsWithJobsetWithQueueWithFailed projectName = leftJoin (projectsWithJobsetWithQueueQuery projectName) nrFailedByJobsetQuery eqName 
    where 
      eqName ((Project{..}, Jobset{..}, _), ( failJobset, failProject, _ )) = (projectName .==  failProject) .&&
                                                   ( (Opaleye.fromNullable  (pgStrictText $ pack "") jobsetName) .==  failJobset) 

nrFailedByJobsetQuery :: Query (Column PGText, Column PGText, Column PGInt8)
nrFailedByJobsetQuery = aggregate (p3(Opaleye.groupBy, Opaleye.groupBy, countStar))  (proc () -> do 
  Build{..} <- queryTable buildTable -< () 
  restrict -< ((buildFinished .== pgInt4 1) .&& ( (Opaleye.fromNullable (pgInt4 0) buildIscurrent) .== pgInt4 1) .&& ((Opaleye.fromNullable (pgInt4 1) buildBuildstatus) ./= pgInt4 0))
  returnA -< (buildJobset, buildProject, buildId)
  )    

projectsWithJobsetNrSucceededQuery :: Text -> Query (ProjectReadColumns, JobsetNullableColumns,
                                                     Column (Nullable PGInt8), Column (Nullable PGInt8), Column (Nullable PGInt8))
projectsWithJobsetNrSucceededQuery projectName = proc() -> do
    ((project, jobset, nrQueued, nrFailed), (_, _, nrSucceeded)) <- (projectsWithJobsetNrSucceeded projectName) -< ()
    returnA -< (project, jobset, nrQueued, nrFailed, nrSucceeded)  

projectsWithJobsetNrSucceeded :: Text -> Query ((ProjectReadColumns, JobsetNullableColumns, Column (Nullable PGInt8), Column (Nullable PGInt8)),
                                                 (Column (Nullable PGText), Column (Nullable PGText), Column (Nullable PGInt8)))
projectsWithJobsetNrSucceeded projectName = leftJoin (projectsWithJobsetWithQueueWithFailedQuery projectName) nrSucceededByJobsetQuery eqName 
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
                              
projectsWithJobsetWithStatus :: Text -> Query ((ProjectReadColumns, JobsetNullableColumns, Column (Nullable PGInt8), Column (Nullable PGInt8), Column (Nullable PGInt8)),
                                                 (Column (Nullable PGText), Column (Nullable PGText), Column (Nullable PGInt4)))
projectsWithJobsetWithStatus projectName = leftJoin (projectsWithJobsetNrSucceededQuery projectName) lastEvaluatedatByJobsetQuery eqName 
  where 
    eqName ((Project{..}, Jobset{..}, _, _, _), ( lastEvalAtJobset, lastEvalAtProject, _ )) = (projectName .==  lastEvalAtProject) .&& ((Opaleye.fromNullable  (pgStrictText $ pack "") jobsetName) .==  lastEvalAtJobset) 

projectsWithJobsetWithStatusQuery :: Text -> Query (ProjectReadColumns, JobsetNullableColumns,
                                                     Column (Nullable PGInt8), Column (Nullable PGInt8), Column (Nullable PGInt8), Column (Nullable PGInt4))
projectsWithJobsetWithStatusQuery projectName = proc() -> do
    ((project, jobset, nrQueued, nrFailed, nrSucceeded), (_, _, lastEvaluatedAt)) <- (projectsWithJobsetWithStatus projectName) -< ()
    returnA -< (project, jobset, nrQueued, nrFailed, nrSucceeded, lastEvaluatedAt)  



lastEvaluatedatByJobsetQuery :: Query (Column PGText, Column PGText, Column PGInt4 )
lastEvaluatedatByJobsetQuery  = aggregate (p3(Opaleye.groupBy, Opaleye.groupBy, Opaleye.max))  (proc () -> do 
                            Jobseteval{..} <- queryTable jobsetevalTable -< () 
                            returnA -< (jobsetevalJobset, jobsetevalProject, jobsetevalTimestamp)
                            )      
