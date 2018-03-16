{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Hercules.Database.Extra
  ( ProjectWithJobsets(..)
  , ProjectWithJobsetsWithStatus(..)
  , JobsetWithStatus(..)
  , JobsetStatus(..)
  , module Hercules.Database.Hydra 
  ) where

import Data.Aeson
import GHC.Generics
import Hercules.Database.Hydra
import Servant.Elm
import Data.Int (Int32, Int64)

data ProjectWithJobsets = ProjectWithJobsets
  { projectWithJobsetsProject :: Project
  , projectWithJobsetsJobsets :: [Jobset]
  }
  deriving(Generic)

data JobsetWithStatus = JobsetWithStatus
  { jobset :: Jobset
  , jobsetStatus :: JobsetStatus
  }  
  deriving(Generic)

data JobsetStatus = JobsetStatus
  { succeed :: Maybe (Int32)
  , failed :: Maybe (Int32)
  , queued :: Maybe (Int64)
  , lastevaluatedAt :: Int32
  }
  deriving(Generic)

data ProjectWithJobsetsWithStatus = ProjectWithJobsetsWithStatus
  { project :: Project
  , jobsets :: [JobsetWithStatus]
  }
  deriving(Generic)

instance ToJSON ProjectWithJobsets where
instance ElmType ProjectWithJobsets where

instance ToJSON JobsetWithStatus where
instance ElmType JobsetWithStatus where

instance ToJSON JobsetStatus where
instance ElmType JobsetStatus where

instance ToJSON ProjectWithJobsetsWithStatus where
instance ElmType ProjectWithJobsetsWithStatus where
