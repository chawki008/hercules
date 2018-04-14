{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Hercules.Controllers.Jobseteval (getJobsetEvals
                                       , getJobsetevalsWithBuilds
                                       ) where

import Hercules.Database.Extra              (JobsetevalinputNullable, Jobseteval, Build, JobsetevalWithBuilds(..)
                                            , BuildNullable, JobsetevalWithStatus(..), Jobsetevalinput, Jobsetevalinput'(..)
                                            , jobsetevalId, fromNullableBuild, fromNullableJobsetevalinput)
import qualified Data.List.NonEmpty as NE
import Data.List                            (sortOn)
import qualified Data.Text as T
import Data.Int                              (Int64)
import Data.Maybe                            (catMaybes, fromMaybe)
import Data.Foldable                         (toList)
import Hercules.Query.Hydra
import Hercules.ServerEnv
import Hercules.Helpers.Helpers


                                
getJobsetevalsWithBuilds :: T.Text -> T.Text -> App [JobsetevalWithBuilds]
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

 
getJobsetEvals :: T.Text -> T.Text -> App [JobsetevalWithStatus]
getJobsetEvals projectNameArg jobsetNameArg = updateChangedInputs <$> (fmap makeJobsetevalWithStatus . groupSortOnJobsetevals jobsetevalId
 <$> (runHydraQueryWithConnection (jobsetevalsWithStatusQuery projectNameArg jobsetNameArg):: App [(Jobseteval, Maybe Int64, Maybe Int64, JobsetevalinputNullable)]))

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
          . sortOn (f . fst4)

updateChangedInputs :: [JobsetevalWithStatus] -> [JobsetevalWithStatus]
updateChangedInputs = fmap (uncurry getChangedInputs) . dupPrev 
       
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

    