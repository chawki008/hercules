{-# LANGUAGE OverloadedStrings #-}

module Hercules.Controllers.QueueSummary (getQueueSummary) where 

import Hercules.Query.Hydra
import Hercules.Database.Extra
import Hercules.ServerEnv
import Safe                                 (headMay)
import Data.Maybe                           (fromMaybe)
import Data.Int                             (Int64)
import Data.Text


getQueueSummary :: App QueueSummary
getQueueSummary = do 
                  jobsetsSummary <- getJobsetsSummary
                  queueLength    <- getQueueLength
                  return QueueSummary { queueSummaryJobsets = jobsetsSummary
                                      , queueSummarySystems = [SystemSummary { systemSummarySystem = "testSystem"
                                                                              , systemSummaryQueued = 2  
                                                                              } 
                                                              ]
                                      , queueSummaryAll = queueLength
                                      , queueSummaryActif = 0
                                      }

getQueueLength :: App Int64
getQueueLength = fromMaybe 0 <$> (headMay <$> runHydraQueryWithConnection queueLengthQuery)

getJobsetsSummary :: App [JobsetSummary]
getJobsetsSummary =  fmap (makeJobsetSummay <$>) (runHydraQueryWithConnection queueLengthByJobsetQuery)    

makeJobsetSummay :: (Text, Text, Int64) -> JobsetSummary
makeJobsetSummay (jobsetname, projectname, queuelength ) = JobsetSummary { jobsetSummaryJobset = jobsetname
                                                                         , jobsetSummaryProject = projectname
                                                                         , jobsetSummaryQueued = queuelength
                                                                         }
