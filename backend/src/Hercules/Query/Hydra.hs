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
  , projectsWithJobsetsQuery
  , jobsetQueueLengthQuery
  , jobsetSucceedFailedLastEvaluatedQuery
  ) where

import Control.Arrow (returnA)
import Data.Text
import Opaleye

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

projectsWithJobsetsQuery
  :: Query (ProjectReadColumns, JobsetNullableColumns)
projectsWithJobsetsQuery = leftJoin projectsQuery jobsetsQuery eqName
  where
    eqName (Project{..}, Jobset{..}) = projectName .== jobsetProject

-- | A query to get the number of succeed and failed builds of the last jobset's evaluation 
jobsetSucceedFailedLastEvaluatedQuery :: Text -> Text -> Query (Column (Nullable PGInt4), Column (Nullable PGInt4), Column PGInt4 )
jobsetSucceedFailedLastEvaluatedQuery jobsetName jobsetProject  = proc () -> do
  Jobseteval{..} <- limit 1 $ (orderBy (desc jobsetevalTimestamp) $ proc () -> do
      jobseteval@Jobseteval{..} <- queryTable jobsetevalTable -< ()
      restrict -< ((jobsetevalJobset .== pgStrictText jobsetName) .&& (jobsetevalProject .== pgStrictText jobsetProject))
      returnA -< jobseteval ) -< ()
  returnA -< (jobsetevalNrsucceeded, jobsetevalNrbuilds, jobsetevalTimestamp)  

-- | A query to get a jobset's query length 
jobsetQueueLengthQuery :: Text -> Text -> Query (Column PGInt8)
jobsetQueueLengthQuery jobsetName jobsetProject = aggregate countStar (proc () -> do
  build@Build{..} <- queryTable buildTable -< ()
  restrict -< ((buildFinished .== pgInt4 0) .&& (buildJobset .== pgStrictText jobsetName ) .&& (buildProject .== pgStrictText jobsetProject ))
  returnA -< build)
  