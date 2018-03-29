module Hercules.Helpers
( makeProjectWithJobsetsWithStatus
, groupSortOnProjectWihtStatus
, getRidOfNullable
, makeJobsetevalWithStatus
, groupSortOnJobsetevals
, updateChangedInputs
) 
where

import Data.List                            (sortOn, elemIndex)
import Data.Foldable                        (toList)
import Data.Int                              (Int32, Int64)
import qualified Data.Text                             as T
import qualified Data.List.NonEmpty as NE
import Data.Maybe                           (catMaybes, fromMaybe)
import Hercules.Database.Extra       (JobsetNullable, Project, JobsetevalinputNullable, fromNullableJobsetevalinput
                                      , JobsetStatus (..), Jobset, Jobseteval, Jobsetevalinput
                                      , JobsetWithStatus (..), ProjectWithJobsetsWithStatus (..), JobsetevalWithStatus(..)
                                      , fromNullableJobset, jobsetevalinputName, jobsetevalinputRevision, jobsetevalinputUri
                                      , jobsetevalinputDependency, jobsetevalinputType )

updateChangedInputs :: [JobsetevalWithStatus] -> [JobsetevalWithStatus]
updateChangedInputs = fmap (uncurry getChangedInputs) . dupPrev 
                                  
                                      
dupPrev :: Eq a => [a] -> [(a, Maybe a)]
dupPrev l = fmap (getWithPrev l) l 

getWithPrev :: Eq a => [a] -> a -> (a, Maybe a)
getWithPrev l element = let 
                            index = fromMaybe 0 $ elemIndex element l 
                        in
                          if index < (length l - 1) then (element, Just (l !!  (index + 1)))
                                        else (element, Nothing)

getChangedInputs :: JobsetevalWithStatus -> Maybe JobsetevalWithStatus -> JobsetevalWithStatus 
getChangedInputs eval1 Nothing = eval1 
getChangedInputs eval1 (Just eval2) = JobsetevalWithStatus { jobseteval = jobseteval eval1
                                                    , jobsetevalSucceeded = jobsetevalSucceeded eval1 
                                                    , jobsetevalQueued = jobsetevalQueued eval1
                                                    , jobsetevalChangedInputs = diffInputs inputs1 inputs2                   
                                                    }
                                                    where 
                                                      inputs1 = jobsetevalChangedInputs eval1
                                                      inputs2 = jobsetevalChangedInputs eval2

diffInputs :: [Jobsetevalinput] -> [Jobsetevalinput] -> [Jobsetevalinput]
diffInputs inputs1 inputs2 = filter changedInputsCompTo2 inputs1
  where 
    changedInputsCompTo2 = changedInputsCompTo inputs2                                                      
                            
changedInputsCompTo :: [Jobsetevalinput] -> Jobsetevalinput -> Bool 
changedInputsCompTo inputs input = changed input $ findInputByName input inputs 

findInputByName :: Jobsetevalinput -> [Jobsetevalinput] -> Maybe Jobsetevalinput
findInputByName input inputs 
            | null input2 = Nothing
            | otherwise = Just (head input2)  
            where
              input2 = filter (hasEqName input) inputs
              hasEqName inputArg i = jobsetevalinputName i == jobsetevalinputName inputArg

changed :: Jobsetevalinput -> Maybe Jobsetevalinput -> Bool
changed _ Nothing = True 
changed input1 (Just input2) = notEqual rev1 rev2 ||  notEqual uri1 uri2 || notEqual dependency1 dependency2 || notEqual type1 type2
            where 
              rev1 = fromMaybe (T.pack "") $ jobsetevalinputRevision input1  
              rev2 = fromMaybe (T.pack "") $ jobsetevalinputRevision input2  
              uri1 = fromMaybe (T.pack "") $ jobsetevalinputUri input1
              uri2 = fromMaybe (T.pack "") $ jobsetevalinputUri input2
              dependency1 = T.pack $ show $ jobsetevalinputDependency input1
              dependency2 = T.pack $ show $ jobsetevalinputDependency input2
              type1 = jobsetevalinputType input1
              type2 = jobsetevalinputType input2
              notEqual el1 el2 = (T.null el1 && el2 /= T.pack "") || (T.null el2 && el1 /= T.pack "") || (el1 /= el2)  

makeProjectWithJobsetsWithStatus :: Project -> [Maybe JobsetWithStatus] -> ProjectWithJobsetsWithStatus
makeProjectWithJobsetsWithStatus p jms =
  let js = catMaybes jms
  in ProjectWithJobsetsWithStatus p js

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

makeJobsetevalWithStatus :: (Jobseteval, Maybe Int64, Maybe Int64, NE.NonEmpty (JobsetevalinputNullable)) -> JobsetevalWithStatus
makeJobsetevalWithStatus (jobsetevalArg, nrSucceeded, nrQueued, inputs) = JobsetevalWithStatus  { jobseteval = jobsetevalArg
                                                                                                , jobsetevalChangedInputs = jobsetevalInputs
                                                                                                , jobsetevalSucceeded = fromMaybe 0 nrSucceeded
                                                                                                , jobsetevalQueued = fromMaybe 0 nrQueued   
                                                                                                }
                                                    where 
                                                      jobsetevalInputs =  catMaybes (fromNullableJobsetevalinput <$> (toList inputs))


groupSortOnJobsetevals :: Ord k => (Jobseteval -> k) -> [(Jobseteval, Maybe Int64, Maybe Int64, JobsetevalinputNullable)] ->
                                               [(Jobseteval, Maybe Int64, Maybe Int64, NE.NonEmpty (JobsetevalinputNullable))]
groupSortOnJobsetevals f = fmap (\x -> (fst4 $ NE.head x, snd4 $ NE.head x, thrd4 $ NE.head x, fmap frth4 x))
          . NE.groupWith (f . fst4)
          . reverse 
          -- . sortOn (f . fst4)
                                                                  
fst4 :: (a,b,c,d) -> a
fst4 (a,_,_,_) = a

snd4 :: (a,b,c,d) -> b
snd4 (_,b,_,_) = b

thrd4 :: (a,b,c,d) -> c
thrd4 (_,_,c,_) = c

frth4 :: (a,b,c,d) -> d
frth4 (_,_,_,d) = d

fst6 :: (a,b,c,d,e,f) -> a
fst6 (a,_,_,_,_,_) = a