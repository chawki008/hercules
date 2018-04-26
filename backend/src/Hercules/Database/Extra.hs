{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings    #-}

module Hercules.Database.Extra
  ( ProjectWithJobsets(..)
  , ProjectWithJobsetsWithStatus(..)
  , JobsetWithStatus(..)
  , JobsetStatus(..)
  , JobsetevalWithStatus(..)
  , JobsetevalWithBuilds(..)
  , JobsetWithInputs (..)
  , QueueSummary (..)
  , JobsetSummary (..)
  , SystemSummary (..)
  , module Hercules.Database.Hydra 
  ) where

import Data.Aeson
import Data.Aeson.Types
import GHC.Generics
import Hercules.Database.Hydra
import Servant.Elm
import Data.Int (Int32, Int64)
import Data.Text
import qualified Data.HashMap.Strict  as M

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

data JobsetWithInputs = JobsetWithInputs 
  { jobsetWithInputsJobset :: Jobset
  , jobsetWithInputsInputs :: [Jobsetinput]
  , jobsetWithInputsInputsalt :: [Jobsetinputalt]
  } 
  deriving (Generic)

data QueueSummary = QueueSummary 
  { queueSummaryJobsets :: [JobsetSummary]
  , queueSummarySystems :: [SystemSummary]
  , queueSummaryAll :: Int64
  , queueSummaryActif :: Int64
  }
  deriving (Generic)

data SystemSummary = SystemSummary
  { systemSummarySystem :: Text
  , systemSummaryQueued :: Int64
  }
  deriving (Generic)

data JobsetSummary = JobsetSummary 
  { jobsetSummaryJobset :: Text 
  , jobsetSummaryProject :: Text
  , jobsetSummaryQueued :: Int64
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

instance ToJSON JobsetWithInputs where
instance ElmType JobsetWithInputs where

instance ToJSON JobsetSummary where
instance ElmType JobsetSummary where
  
instance ToJSON SystemSummary where
instance ElmType SystemSummary where

instance ToJSON QueueSummary where
instance ElmType QueueSummary where

instance FromJSON JobsetWithInputs where
  parseJSON j = do
			jobsetWithInputs 		 <- parseJSON j 
			jobset					     <- getJobset jobsetWithInputs
			jsonInputs 				   <- jobsetWithInputs .: "inputs"	
			inputs 			 		     <- getInputsFromJSON (jobsetProject jobset) (jobsetName jobset) jsonInputs
			return JobsetWithInputs { jobsetWithInputsJobset = jobset
                              , jobsetWithInputsInputs = fst inputs
                              , jobsetWithInputsInputsalt = snd inputs
                              }
									 
				   
getJobset :: Object -> Parser Jobset
getJobset jobsetWithInputs = do 
						jobsetName		 		        <- jobsetWithInputs .:? "name" .!= "tmpJobsetName"
						jobsetProject	 		        <- jobsetWithInputs .:? "project" .!= "tmpProject" 
						jobsetErrormsg	 		      <- jobsetWithInputs .:? "errormsg" ::  Parser (Maybe Text)
						jobsetErrortime	 		      <- jobsetWithInputs .:? "errortime" :: Parser (Maybe Int32)
						jobsetLastcheckedtime	    <- jobsetWithInputs .:? "lastcheckedtime" :: Parser (Maybe Int32)
						jobsetTriggertime    	    <- jobsetWithInputs .:? "triggertime" :: Parser (Maybe Int32)
						jobsetFetcherrormsg    	  <- jobsetWithInputs .:? "fetcherrormsg" :: Parser (Maybe Text)
						jobsetEnabled	 		        <- jobsetWithInputs .: "enabled" :: Parser Int32
						jobsetHidden	 		        <- jobsetWithInputs .: "hidden" :: Parser Bool
						jobsetDescription		      <- jobsetWithInputs .:? "description" :: Parser (Maybe Text)
						jobsetNixexprinput		    <- jobsetWithInputs .: "nixexprinput" :: Parser Text
						jobsetCheckinterval		    <- jobsetWithInputs .: "checkinterval" :: Parser Int32
						jobsetSchedulingshares	  <- jobsetWithInputs .: "schedulingshares" :: Parser Int32
						jobsetEnableemail		      <- jobsetWithInputs .: "enableemail" :: Parser Bool
						jobsetEmailoverride		    <- jobsetWithInputs .: "emailoverride" :: Parser Text
						jobsetNixexprpath		      <- jobsetWithInputs .: "nixexprpath" :: Parser Text
						jobsetKeepnr		 	        <- jobsetWithInputs .: "keepnr" :: Parser Int32
						return Jobset { jobsetName = jobsetName
                          , jobsetProject = jobsetProject
                          , jobsetNixexprpath = jobsetNixexprpath
                          , jobsetErrormsg = jobsetErrormsg
                          , jobsetErrortime = jobsetErrortime
                          , jobsetLastcheckedtime = jobsetLastcheckedtime
                          , jobsetFetcherrormsg = jobsetFetcherrormsg
                          , jobsetTriggertime = jobsetTriggertime
                          , jobsetEnabled = jobsetEnabled
                          , jobsetHidden = if jobsetHidden then 1 else 0 
                          , jobsetDescription = jobsetDescription
                          , jobsetNixexprinput = jobsetNixexprinput
                          , jobsetCheckinterval = jobsetCheckinterval
                          , jobsetSchedulingshares = jobsetSchedulingshares
                          , jobsetEnableemail = if jobsetEnableemail then 1 else 0 
                          , jobsetEmailoverride = jobsetEmailoverride
                          , jobsetKeepnr = jobsetKeepnr
                          , jobsetForceeval = Nothing
                          }

getInputsFromJSON ::   Text -> Text -> Value -> Parser ([Jobsetinput], [Jobsetinputalt])
getInputsFromJSON projectName jobsetName = withObject "set of inputs" $ \inputsObject -> do
                                                                            inputsNames         <- return (M.keys (inputsObject :: Object)) 
                                                                            inputsAndInputsalts <- mapM (getInput inputsObject projectName jobsetName) inputsNames
                                                                            return (Prelude.unzip inputsAndInputsalts)
		
getInput :: Object -> Text -> Text -> Text -> Parser (Jobsetinput, Jobsetinputalt)
getInput inputsObject projectName jobsetName inputName = do 
								inputInfos    <- inputsObject .: inputName
								inputType     <- inputInfos .: "type" :: Parser Text
								inputEmail    <- inputInfos .: "emailresponsible" :: Parser Bool
								inputValue    <- inputInfos .:? "value" :: Parser (Maybe Text)
								inputRevision <- inputInfos .:? "revision" :: Parser (Maybe Text)
								return ( Jobsetinput  { jobsetinputProject          = projectName
                                      , jobsetinputJobset           = jobsetName
                                      , jobsetinputName             = inputName
                                      , jobsetinputType             = inputType
                                      , jobsetinputEmailresponsible = if inputEmail then 1 else 0 
                                      }, Jobsetinputalt	{ jobsetinputaltProject  = projectName
                                                        , jobsetinputaltJobset   = jobsetName
                                                        , jobsetinputaltInput    = inputName
                                                        , jobsetinputaltAltnr    = 0
                                                        , jobsetinputaltValue    = inputValue
                                                        , jobsetinputaltRevision = inputRevision
                                                        })

