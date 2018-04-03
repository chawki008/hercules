{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Hercules.Database.Extra
  ( ProjectWithJobsets(..)
  , ProjectWithJobsetsWithStatus(..)
  , JobsetWithStatus(..)
  , JobsetStatus(..)
  , JobsetevalWithStatus(..)
  , JobsetevalWithBuilds(..)
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
  { jobsetNrsucceeded ::  Int64
  , jobsetNrfailed ::  Int64
  , jobsetNrqueued ::  Int64
  , jobsetLastevaluatedAt :: Maybe Int32
  }
  deriving(Generic)

data ProjectWithJobsetsWithStatus = ProjectWithJobsetsWithStatus
  { project :: Project
  , jobsets :: [JobsetWithStatus]
  }
  deriving(Generic)

data JobsetevalWithStatus = JobsetevalWithStatus
  { jobseteval :: Jobseteval
  , jobsetevalChangedInputs :: [Jobsetevalinput]
  , jobsetevalSucceeded :: Int64
  , jobsetevalQueued :: Int64  
  }
  deriving(Generic, Eq)

data JobsetevalWithBuilds = JobsetevalWithBuilds
  { jobsetevalWithBuildsEval :: Jobseteval 
  , builds :: [Build]
  }
  deriving (Generic)


instance ToJSON ProjectWithJobsets where
instance ElmType ProjectWithJobsets where

instance ToJSON JobsetWithStatus where
instance ElmType JobsetWithStatus where

instance ToJSON JobsetStatus where
instance ElmType JobsetStatus where

instance ToJSON ProjectWithJobsetsWithStatus where
instance ElmType ProjectWithJobsetsWithStatus where

instance ToJSON JobsetevalWithStatus where
instance ElmType JobsetevalWithStatus where

instance ToJSON JobsetevalWithBuilds where
instance ElmType JobsetevalWithBuilds where
