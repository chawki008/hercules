{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Hercules.Controllers.Jobset (addJobset
                                   , addJobsetWithInputs 
                                   ) where 

import Data.Text
import Hercules.Controllers.Project  (getProjectWithJobsetsWithStatus)
import Hercules.Helpers.Helpers      (toMaybe)
import Servant
import Hercules.Query.Hydra
import Safe                          (headMay)
import Hercules.ServerEnv
import Data.Int                      (Int32)
import Opaleye
import Hercules.Database.Extra       ( Jobset'(..), Jobset, JobsetWithStatus, Job
                                     , JobsetWithInputs(..), Jobsetinput, Jobsetinputalt, Jobsetinput'(..), Jobsetinputalt'(..)
                                     , JobsetWriteColumns, JobsetinputWriteColumns, JobsetinputaltWriteColumns
                                     , jobset, jobsetTable, jobsetinputTable, jobsetinputaltTable, jobsets)
                        
import Hercules.Helpers.JSONParsers()

addJobsetWithInputs :: Text -> JobsetWithInputs -> App Text
addJobsetWithInputs projectNameArg jobsetWithInputs = do 
    mProject <- getProjectWithJobsetsWithStatus projectNameArg
    mJobset  <- case mProject of 
                  Just project -> return $ headMay $ Prelude.filter (eqJobset (jobsetWithInputsJobset jobsetWithInputs)) (jobsets project)                                    
                  Nothing -> throwError $ err404 { errBody = "Project doesn't exist" } 
    case mJobset of 
        Just _ ->  throwError $ err409 { errBody = "jobset already exists" } 
        Nothing -> validateJobsetWithInputs jobsetWithInputs
    runHydraUpdateWithConnection jobsetTable [(constantJobset (jobsetWithInputsJobset jobsetWithInputs)  projectNameArg)]
    runHydraUpdateWithConnection jobsetinputTable (fmap (constantJobsetinput projectNameArg) (jobsetWithInputsInputs jobsetWithInputs))
    runHydraUpdateWithConnection jobsetinputaltTable (fmap (constantJobsetinputalt projectNameArg) (jobsetWithInputsInputsalt jobsetWithInputs))
    return ("Jobset added successfully") 

addJobset :: Text -> Jobset -> App (Text)
addJobset projectNameArg jobset =  do 
    mProject <- getProjectWithJobsetsWithStatus projectNameArg
    mJobset  <- case mProject of 
                  Just project -> return $ headMay $ Prelude.filter (eqJobset jobset) (jobsets project)                                    
                  Nothing -> throwError $ err404 { errBody = "Project doesn't exist" } 
    case mJobset of 
        Just _ ->  throwError $ err409 { errBody = "jobset already exists" } 
        Nothing -> runHydraUpdateWithConnection jobsetTable [(constantJobset jobset projectNameArg)]
    return ("Jobset added successfully") 
         
eqJobset :: Jobset -> JobsetWithStatus -> Bool
eqJobset jobset1 jobset2 = jobsetName jobset1 == (jobsetName $ jobset jobset2)

validateJobsetWithInputs :: JobsetWithInputs -> App Text
validateJobsetWithInputs jobsetWithInput = do 
                                  checkName    jobsetname
                                  checkEnabled jobsetenabled
                                  checkShares  jobsetshares
                                  checkNixExprAndPath jobsetnixexprinput jobsetnixexprpath
                                  checkJobsetinputValue jobsetWithInput
                                  return ("Ok")
                              where 
                                  jobsetInputJobset = jobsetWithInputsJobset jobsetWithInput
                                  jobsetname = jobsetName jobsetInputJobset
                                  jobsetenabled = jobsetEnabled jobsetInputJobset
                                  jobsetshares = jobsetSchedulingshares jobsetInputJobset
                                  jobsetnixexprinput = jobsetNixexprinput jobsetInputJobset
                                  jobsetnixexprpath = jobsetNixexprpath jobsetInputJobset
checkName :: Text -> App Text
checkName jobsetname = return ("Ok") :: App Text

checkEnabled :: Int32 -> App Text
checkEnabled jobsetenabled = do
                      if jobsetenabled > 2 || jobsetenabled < 0 
                        then throwError $ err412 { errBody = "jobset enabled field must be between 0 and 2 !" }   
                        else return ("Ok") :: App Text

checkShares :: Int32 -> App Text
checkShares jobsetshares = return ("Ok") :: App Text

checkNixExprAndPath :: Text -> Text -> App Text
checkNixExprAndPath jobsetnixexprinput jobsetnixexprpath = return ("Ok") :: App Text

checkJobsetinputValue :: JobsetWithInputs -> App Text
checkJobsetinputValue jobsetWithInput = return ("Ok") :: App Text

constantJobset :: Jobset -> Text -> JobsetWriteColumns
constantJobset jobset jobsetProjectName =  Jobset { jobsetName             =  constant $ jobsetName jobset      
                                                  , jobsetProject          =  constant $ jobsetProjectName
                                                  , jobsetDescription      =  constant $ toMaybe $ jobsetDescription jobset
                                                  , jobsetNixexprinput     =  constant $ jobsetNixexprinput jobset
                                                  , jobsetNixexprpath      =  constant $ jobsetNixexprpath jobset
                                                  , jobsetErrormsg         =  constant $ toMaybe $ jobsetErrormsg jobset
                                                  , jobsetErrortime        =  constant $ toMaybe $ jobsetErrortime jobset
                                                  , jobsetLastcheckedtime  =  constant $ toMaybe $ jobsetLastcheckedtime jobset
                                                  , jobsetTriggertime      =  constant $ toMaybe $ jobsetTriggertime jobset
                                                  , jobsetEnabled          =  constant $ jobsetEnabled jobset
                                                  , jobsetEnableemail      =  constant $ jobsetEnableemail jobset
                                                  , jobsetHidden           =  constant $ jobsetHidden jobset
                                                  , jobsetEmailoverride    =  constant $ jobsetEmailoverride jobset
                                                  , jobsetKeepnr           =  constant $ jobsetKeepnr jobset
                                                  , jobsetCheckinterval    =  constant $ jobsetCheckinterval jobset
                                                  , jobsetSchedulingshares =  constant $ jobsetSchedulingshares jobset
                                                  , jobsetFetcherrormsg    =  constant $ toMaybe $ jobsetFetcherrormsg jobset
                                                  }   

constantJobsetinputalt ::Text -> Jobsetinputalt -> JobsetinputaltWriteColumns
constantJobsetinputalt jobsetinputaltProjectName jobsetinputalt = Jobsetinputalt  { jobsetinputaltAltnr    =  constant $ jobsetinputaltAltnr jobsetinputalt      
                                                                                  , jobsetinputaltProject  =  constant $ jobsetinputaltProjectName
                                                                                  , jobsetinputaltJobset   =  constant $ jobsetinputaltJobset jobsetinputalt
                                                                                  , jobsetinputaltInput    =  constant $ jobsetinputaltInput jobsetinputalt
                                                                                  , jobsetinputaltValue    =  constant $ toMaybe $ jobsetinputaltValue jobsetinputalt
                                                                                  , jobsetinputaltRevision =  constant $ toMaybe $ jobsetinputaltRevision jobsetinputalt
                                                                                  }   

constantJobsetinput :: Text -> Jobsetinput -> JobsetinputWriteColumns
constantJobsetinput jobsetinputProjectName jobsetinput =  Jobsetinput { jobsetinputProject           =  constant $ jobsetinputProjectName
                                                                      , jobsetinputJobset            =  constant $ jobsetinputJobset jobsetinput
                                                                      , jobsetinputName              =  constant $ jobsetinputName jobsetinput     
                                                                      , jobsetinputType              =  constant $ jobsetinputType jobsetinput
                                                                      , jobsetinputEmailresponsible  =  constant $ jobsetinputEmailresponsible jobsetinput
                                                                      }   
                                                  
getJobsetJobs :: Text -> Text -> App [Job]
getJobsetJobs projectNameArg jobsetNameArg = runHydraQueryWithConnection (jobsetjobsQuery projectNameArg jobsetNameArg)

