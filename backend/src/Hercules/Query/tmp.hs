jobsetsQuery = proc () -> do
    jobset@Jobset{..} <- queryTable jobsetTable -< () 
    restrict -< jobsetEnabled ./= pgInt4 0
    returnA -< jobset



getRidOfNullable :: (Project, JobsetNullable, Maybe Int64)  -> (Project, Maybe Jobset, Int64)
getRidOfNullable (p, j, count) = (p, js, fromMaybe 0 count )
  where js = fromNullableJobset j

getRidOfNullable1 :: (Project, JobsetNullable, Maybe Int64, Maybe Int64)  -> (Project, Maybe Jobset, Int64, Int64)
getRidOfNullable1 (p, j, queued, failed) = (p, js, fromMaybe 0 queued, fromMaybe 0 failed)
  where js = fromNullableJobset j

getTest :: App [(Text, Text, Int64)]
getTest = runHydraQueryWithConnection queueLengthByJobsetQuery

getTestProJobSche :: App [(Project, Maybe Jobset, Int64)]
getTestProJobSche = (getRidOfNullable <$>) <$> (runHydraQueryWithConnection projectsWithJobsetWithQueueQuery :: App [(Project, JobsetNullable, Maybe Int64 )])

getTestProJobScheFail :: App [(Project, Maybe Jobset, Int64, Int64)]
getTestProJobScheFail = (getRidOfNullable1 <$>) <$> (runHydraQueryWithConnection projectsWithJobsetWithQueueWithFailedQuery :: App [(Project, JobsetNullable, Maybe Int64, Maybe Int64 )])




-- query/hydra ---------------------------------------------------------------------------------------

-- | A query to get the number of succeeded and failed builds of the last jobset's evaluation 
lastEvaluatedatByJobsetQuery :: Query (Column PGText, Column PGText, Column PGInt4 )
lastEvaluatedatByJobsetQuery  = proc () -> do
  Jobseteval{..} <- limit 1 $ (orderBy (desc jobsetevalTimestamp) $ proc () -> do
      jobseteval@Jobseteval{..} <- queryTable jobsetevalTable -< ()
      restrict -< ((jobsetevalJobset .== pgStrictText jobsetName) .&& (jobsetevalProject .== pgStrictText jobsetProject))
      returnA -< jobseteval ) -< ()
  returnA -< ( jobsetevalJobset, jobsetevalProject, jobsetevalTimestamp)  

-- | A query to get a jobset's query length 
jobsetQueueLengthQuery :: Text -> Text -> Query (Column PGInt8)
jobsetQueueLengthQuery jobsetName jobsetProject = aggregate countStar (proc () -> do
  build@Build{..} <- queryTable buildTable -< ()
  restrict -< ((buildFinished .== pgInt4 0) .&& (buildJobset .== pgStrictText jobsetName ) .&& (buildProject .== pgStrictText jobsetProject ))
  returnA -< build)
