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
  , projectsWithJobsetWithStatusQuery
  , jobsetjobsQuery
  , jobsetevalsWithStatusQuery
  , jobsetevalsWithBuildsQuery
  , queueLengthByJobsetQuery
  , queueLengthQuery
  ) where

import Control.Arrow (returnA)
import Data.Text
import Opaleye
import Hercules.Database.Hydra
import Hercules.Query.Jobsets
import Hercules.Query.Jobsetevals

-- query to get all jobset's jobs
jobsetjobsQuery :: Text -> Text -> Query JobReadColumns     
jobsetjobsQuery projectName jobsetName = proc () -> do 
  job@Job{..} <- queryTable jobTable -< ()
  restrict -< ((jobJobset .== pgStrictText jobsetName) .&& (jobProject .== pgStrictText projectName))
  returnA -< job  

queueLengthQuery :: Query (Column PGInt8)
queueLengthQuery = aggregate countStar (proc () -> do
  build@Build{..} <- queryTable buildTable -< ()
  restrict -< (buildFinished .== pgInt4 0)
  returnA -< build)