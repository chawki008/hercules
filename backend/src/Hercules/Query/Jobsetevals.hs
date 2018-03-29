{-# LANGUAGE Arrows          #-}
{-# LANGUAGE RecordWildCards #-}


{-|
A module to handle the different subqueries we might want to make to Hydra's
database concerning jobsetevals
-}
module Hercules.Query.Jobsetevals
  ( jobsetevalsWithStatusQuery
  ) where


import Control.Arrow (returnA)
import Data.Text
import Opaleye
import Data.Profunctor.Product (p2)
import Hercules.Database.Hydra

-- query to get a jobset's eval within its status ( nrSucceeded, nrQueued, jobsetevalInputs) given the project and jobset names
jobsetevalsWithStatusQuery :: Text -> Text -> Query (JobsetevalReadColumns, Column (Nullable PGInt8), Column (Nullable PGInt8), JobsetevalinputNullableColumns)
jobsetevalsWithStatusQuery projectNameArg jobsetNameArg = proc () -> do 
 ((jobseteval, nrSucc, nrQueu), (_, jobsetevalInputs)) <- ((leftJoin infosQuery inputsQuery eqName) :: Query ((JobsetevalReadColumns,
                                                                     Column (Nullable PGInt8), Column (Nullable PGInt8)),(JobsetevalNullableColumns, JobsetevalinputNullableColumns))) -< ()
 returnA -< (jobseteval, nrSucc, nrQueu, jobsetevalInputs)
    where 
      inputsQuery = jobsetevalInputsByjobsetevalQuery jobsetevalQ
      infosQuery = jobsetevalsWithInfosQuery jobsetevalQ
      jobsetevalQ = jobsetevalsQuery projectNameArg jobsetNameArg
      eqName ((jobseteval1, _, _), (jobseteval2, _)) = jobsetevalId jobseteval1 .== jobsetevalId jobseteval2 

-- query to get 11 last jobset's evals
jobsetevalsQuery :: Text -> Text -> Query JobsetevalReadColumns     
jobsetevalsQuery projectNameArg jobsetNameArg = proc () -> do  
  jobseteval@Jobseteval{..} <- limit 11 $ (orderBy (desc jobsetevalId) $ proc () -> do 
                jobseteval@Jobseteval{..} <- queryTable jobsetevalTable -< ()
                restrict -< ((jobsetevalJobset .== pgStrictText jobsetNameArg) .&& (jobsetevalProject .== pgStrictText projectNameArg) .&& (jobsetevalHasnewbuilds .== pgInt4 1))
                returnA -< jobseteval  ) -< () 

  returnA -< jobseteval         
      
-- query to get all jobsetevalinputs given a jobseteval query 
jobsetevalInputsByjobsetevalQuery :: Query JobsetevalReadColumns -> Query (JobsetevalReadColumns ,JobsetevalinputNullableColumns) 
jobsetevalInputsByjobsetevalQuery jobsetevalQ = leftJoin jobsetevalQ jobsetevalinputQuery eqName
  where 
    eqName (Jobseteval{..}, Jobsetevalinput{..}) = jobsetevalId .== jobsetevalinputEval 

-- query to get all jobsetevalinputs that have build dependencies or both uri and revision defined
jobsetevalinputQuery :: Query JobsetevalinputReadColumns
jobsetevalinputQuery = proc () -> do 
  jobsetevalinput@Jobsetevalinput{..} <- queryTable jobsetevalinputTable -< ()
  restrict -< (jobsetevalinputAltnr .== (pgInt4 0)) .&& (((oNot $ oIsNull jobsetevalinputUri) .&& (oNot $ oIsNull jobsetevalinputRevision)) .|| (oNot $ oIsNull jobsetevalinputDependency))
  returnA -< jobsetevalinput
  where 
    oIsNull = Opaleye.isNull
    oNot = Opaleye.not

-- query to get jobseteval within its infos (nrSucceeded, nrQueued) given a jobseteval query (returns Query (jobseteval, nrSucceeded, nrQueued))
jobsetevalsWithInfosQuery :: Query JobsetevalReadColumns -> Query (JobsetevalReadColumns, Column (Nullable PGInt8), Column (Nullable PGInt8))
jobsetevalsWithInfosQuery jobsetevalQ = proc () -> do 
 ((jobseteval, nrSucc), (_, nrQueu)) <- ((leftJoin succQuery queueQuery eqName) :: Query((JobsetevalReadColumns, Column (Nullable PGInt8)),
                                                                                               (JobsetevalNullableColumns, Column (Nullable PGInt8)))) -< ()
 returnA -< (jobseteval, nrSucc, nrQueu)
          where 
            queueQuery = aJobsetevalsWithInfoQuery jobsetevalQ nrQueuedInfo
            succQuery = aJobsetevalsWithInfoQuery jobsetevalQ nrSuccInfo
            nrSuccInfo = nrSucceededByJobsetEval $ jobsetevalQ
            nrQueuedInfo = nrQueuedByJobsetEval $ jobsetevalQ
            eqName ((jobseteval1, _), (jobseteval2, _)) = jobsetevalId jobseteval1 .== jobsetevalId jobseteval2 
    

-- takes a jobsetevals query and an info(ex : nrSucceeded, nrQueued ... ) query and join them  (returns Query (jobseteval, info))
aJobsetevalsWithInfoQuery ::  Query JobsetevalReadColumns -> Query (Column PGInt4, Column PGInt8) -> Query (JobsetevalReadColumns, Column (Nullable PGInt8))
aJobsetevalsWithInfoQuery aJobsetevalQuery infoByJobsetEval = proc() -> do 
  (jobseteval, (_, jobsetevalNrsucceeded)) <- (aJobsetevalsWithInfo aJobsetevalQuery infoByJobsetEval ) -< ()
  returnA -< (jobseteval, jobsetevalNrsucceeded)

aJobsetevalsWithInfo :: Query JobsetevalReadColumns -> Query (Column PGInt4, Column PGInt8) -> 
                               Query (JobsetevalReadColumns, (Column (Nullable PGInt4), Column (Nullable PGInt8)))
aJobsetevalsWithInfo aJobsetevalQuery infoByJobsetEval = leftJoin aJobsetevalQuery  infoByJobsetEval eqName
  where 
    eqName (Jobseteval{..}, (jobsetevalwithInfoId, _)) = jobsetevalId .== jobsetevalwithInfoId

-- query to get number of succeeded builds by jobseteval (returns Query (jobsetevalId, nrSucceeded))
nrSucceededByJobsetEval :: Query JobsetevalReadColumns -> Query (Column PGInt4, Column PGInt8)
nrSucceededByJobsetEval aJobsetevalQuery = aggregate (p2(Opaleye.groupBy, countStar))  (proc () -> do 
  (Jobseteval{..}, Build{..}) <- (jobsetevalsBuildsQuery aJobsetevalQuery)-< () 
  restrict -< (( fromNull (pgInt4 0 ) buildFinished .== pgInt4 1) .&& ( (fromNull (pgInt4 1) buildBuildstatus) .== pgInt4 0) )
  returnA -< (jobsetevalId, buildId)
  )
  where 
    fromNull = Opaleye.fromNullable 
    
-- query to get number of queued builds by jobseteval (returns Query (jobsetevalId, nrQueued))
nrQueuedByJobsetEval :: Query JobsetevalReadColumns -> Query (Column PGInt4, Column PGInt8)
nrQueuedByJobsetEval aJobsetevalQuery = aggregate (p2(Opaleye.groupBy, countStar))  (proc () -> do 
  (Jobseteval{..}, Build{..}) <- (jobsetevalsBuildsQuery aJobsetevalQuery)-< () 
  restrict -< (fromNull (pgInt4 1) buildFinished .== pgInt4 0) 
  returnA -< (jobsetevalId, buildId)
  )
  where 
    fromNull = Opaleye.fromNullable 

-- query to get jobsetevals with associated builds (join jobsetevals builds) (returns Query (jobseteval, build))
jobsetevalsBuildsQuery :: Query (JobsetevalReadColumns) -> Query (JobsetevalReadColumns, BuildNullableColumns)
jobsetevalsBuildsQuery aJobsetevalQuery = proc () -> do 
  ((jobseteval, _), build) <- (jobsetevalsBuilds aJobsetevalQuery) -< ()
  returnA -< (jobseteval, build)

jobsetevalsBuilds :: Query (JobsetevalReadColumns)  -> Query ((JobsetevalReadColumns, Column (Nullable PGInt4)), BuildNullableColumns)    
jobsetevalsBuilds aJobsetevalQuery = leftJoin (jobsetevalsBuildsMembersQuery aJobsetevalQuery) buildsQuery eqName 
    where eqName ((Jobseteval{..}, jobsetevalbuildId), Build{..}) =  Opaleye.fromNullable (pgInt4 0) jobsetevalbuildId .== buildId

-- query to get jobsetevals with associated builds ids (join jobsetevals jobsetevalmembers) (returns Query (jobseteval, buildId))
jobsetevalsBuildsMembersQuery :: Query (JobsetevalReadColumns) -> Query (JobsetevalReadColumns , Column (Nullable PGInt4))    
jobsetevalsBuildsMembersQuery aJobsetevalQuery = proc () -> do 
  (jobseteval, Jobsetevalmember{..}) <- (jobsetevalsBuildsIds aJobsetevalQuery) -< ()
  returnA -< (jobseteval, jobsetevalmemberBuild)

jobsetevalsBuildsIds :: Query (JobsetevalReadColumns) -> Query (JobsetevalReadColumns , JobsetevalmemberNullableColumns)
jobsetevalsBuildsIds aJobsetevalQuery = leftJoin aJobsetevalQuery jobsetevalmembersQuery eqName
    where 
      eqName (Jobseteval {..}, Jobsetevalmember{..}) = jobsetevalId .== jobsetevalmemberEval

-- | A query to get a list of all builds
buildsQuery :: Query BuildReadColumns
buildsQuery = queryTable buildTable

-- | A query to get a list of all jobsetevalmembers
jobsetevalmembersQuery :: Query JobsetevalmemberReadColumns
jobsetevalmembersQuery = queryTable jobsetevalmemberTable
