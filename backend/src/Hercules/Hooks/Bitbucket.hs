{-# LANGUAGE OverloadedStrings #-}


module Hercules.Hooks.Bitbucket
( handlePR
) where 

    
import qualified Data.ByteString.Lazy as BS
import Data.Text 
import Control.Monad.Reader
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Client
import Control.Monad.IO.Class ( liftIO )
import Hercules.ServerEnv
import Data.Text.Encoding     (decodeUtf8)  
import Hercules.Database.Extra 
import Hercules.Database.Hercules
import Hercules.Query.Jobsets (projectByRepoQuery)
import Data.Aeson (eitherDecode)
import Hercules.Controllers.Jobset          (addJobsetWithInputs)
import Servant
import Data.ByteString.Lazy                 (fromStrict)
import Safe                                 (headMay)
import Hercules.Config
import Data.Time.Clock.POSIX 

getFile :: Text -> Text -> BS.ByteString -> App (BS.ByteString)
getFile fileName repo revision = do 
                            manager   <- liftIO $ newManager tlsManagerSettings
                            request_  <- return $ parseRequest_ $ unpack $ intercalate "" ["https://api.bitbucket.org/2.0/repositories/", repo, "/src/", decodeUtf8 $ BS.toStrict revision, "/", fileName]
                            mAuthInfo <- asks envBitbucketAuthInfo
                            case mAuthInfo of 
                                Just authInfo -> do 
                                    username  <- return $ basicAuthInfoUsername authInfo
                                    password  <- return $ basicAuthInfoPassword authInfo
                                    request   <- return $ applyBasicAuth username password request_
                                    response  <- liftIO $ httpLbs request manager  
                                    return $ responseBody response
                                Nothing ->  return "No credentials were provided"   

handlePR :: BitbucketPR -> App (Text)
handlePR pullrequest = do     
                    repo             <- return $ bitbucketPRRepo pullrequest
                    projectExist     <- checkRepoProject repo
                    project          <- case projectExist of 
                                          Just project -> return project
                                          Nothing -> throwError $ err404 { errBody = "no project is associated with this repo" }
                    repoConfig       <- getFile "hercules.json" repo $ (fromStrict . bitbucketPRCommit) pullrequest                     
                    eJobset          <- return (eitherDecode repoConfig  :: Either String JobsetWithInputs) 
                    namelessJobset   <- case eJobset of 
                                          Right jobsetWithInputs -> return $ addBranch jobsetWithInputs repo $ bitbucketPRHeadbranch pullrequest
                                          Left error -> throwError $ err412 { errBody = "failed getting hercules.json file" }
                    jobset           <- assignNameAndForceEval namelessJobset (projectName project) $ (pack . show) $ bitbucketPRId pullrequest             
                    addJobsetWithInputs (projectName project) jobset
                    return "Ok"
                    
checkRepoProject :: Text -> App (Maybe Project) 
checkRepoProject repo = headMay <$> runHydraQueryWithConnection (projectByRepoQuery repo)



addBranch :: JobsetWithInputs -> Text -> Text -> JobsetWithInputs
addBranch jobsetWithInputs repo branch = jobsetWithInputs 

assignNameAndForceEval :: JobsetWithInputs -> Text -> Text -> App (JobsetWithInputs) 
assignNameAndForceEval namelessjobset projectname prid = do
                                                forcedJobset <- assignNametoJobsetAndFroceEval (jobsetWithInputsJobset namelessjobset) projectname prid
                                                return namelessjobset   { jobsetWithInputsJobset = forcedJobset
                                                                        , jobsetWithInputsInputs = assignNametoInputs (jobsetWithInputsInputs namelessjobset) projectname prid
                                                                        , jobsetWithInputsInputsalt = assignNametoInputalts (jobsetWithInputsInputsalt namelessjobset) projectname prid
                                                                        } 

assignNametoJobsetAndFroceEval :: Jobset -> Text -> Text -> App (Jobset)
assignNametoJobsetAndFroceEval jobset projectname prid = do
                                                    currentTimestamp <- liftIO $ round `fmap` getPOSIXTime 
                                                    return jobset  { jobsetName = createJobsetPrName projectname prid 
                                                                   , jobsetForceeval = Just True
                                                                   , jobsetTriggertime = Just currentTimestamp
                                                                   }

assignNametoInputs :: [Jobsetinput] -> Text -> Text -> [Jobsetinput]
assignNametoInputs jobsetinputs projectname prid =   (\jobsetinput -> jobsetinput { jobsetinputJobset = createJobsetPrName projectname prid } ) <$> jobsetinputs

assignNametoInputalts :: [Jobsetinputalt] -> Text -> Text -> [Jobsetinputalt]
assignNametoInputalts jobsetinputalts projectname prid = (\jobsetinputalt -> jobsetinputalt { jobsetinputaltJobset = createJobsetPrName projectname prid } ) <$> jobsetinputalts

createJobsetPrName :: Text -> Text -> Text
createJobsetPrName projectname prid = intercalate "_pr_" [projectname, prid]
