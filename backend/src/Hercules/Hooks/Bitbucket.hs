{-# LANGUAGE OverloadedStrings #-}


module Hercules.Hooks.Bitbucket
( handlePR
) where 

    
import qualified Data.ByteString.Lazy as BS
import Data.Text 
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

getFile :: Text -> Text -> BS.ByteString -> App (BS.ByteString)
getFile fileName repo revision = do 
                            manager  <- liftIO $ newManager tlsManagerSettings
                            request_ <- return $ parseRequest_ $ unpack $ intercalate "" ["https://api.bitbucket.org/2.0/repositories/", repo, "/src/", decodeUtf8 $ BS.toStrict revision, "/", fileName]
                            request  <- return $ applyBasicAuth "username" "password" request_
                            response <- liftIO $ httpLbs request manager  
                            return $ responseBody response

handlePR :: BitbucketPR -> App (Text)
handlePR pullrequest = do     
                    repo             <- return $ bitbucketPRRepo pullrequest
                    projectExist     <- checkRepoProject repo
                    project          <- case projectExist of 
                                          Just project -> return project
                                          Nothing -> throwError $ err404 { errBody = "no project is associated with this repo" }
                    repoConfig       <- getFile "hydra.json" repo $ (fromStrict . bitbucketPRCommit) pullrequest                     
                    eJobset          <- return (eitherDecode repoConfig  :: Either String JobsetWithInputs) 
                    namelessJobset   <- case eJobset of 
                                          Right jobsetWithInputs -> return $ addBranch jobsetWithInputs repo $ bitbucketPRHeadbranch pullrequest
                                          Left error -> throwError $ err412 { errBody = "error" }
                    jobset           <- return $ assignNameAndForceEval namelessJobset (projectName project) $ (pack . show) $ bitbucketPRId pullrequest             
                    addJobsetWithInputs (projectName project) jobset
                    liftIO $ putStrLn $ show pullrequest
                    return "Ok"
                    
checkRepoProject :: Text -> App (Maybe Project) 
checkRepoProject repo = headMay <$> runHydraQueryWithConnection (projectByRepoQuery repo)



addBranch :: JobsetWithInputs -> Text -> Text -> JobsetWithInputs
addBranch jobsetWithInputs repo branch = jobsetWithInputs 

assignNameAndForceEval :: JobsetWithInputs -> Text -> Text -> JobsetWithInputs 
assignNameAndForceEval namelessjobset projectname prid = namelessjobset{ jobsetWithInputsJobset = assignNametoJobsetAndFroceEval (jobsetWithInputsJobset namelessjobset) projectname prid
                                                           , jobsetWithInputsInputs = assignNametoInputs (jobsetWithInputsInputs namelessjobset) projectname prid
                                                           , jobsetWithInputsInputsalt = assignNametoInputalts (jobsetWithInputsInputsalt namelessjobset) projectname prid
                                                           } 

assignNametoJobsetAndFroceEval :: Jobset -> Text -> Text -> Jobset
assignNametoJobsetAndFroceEval jobset projectname prid = jobset { jobsetName = intercalate "_pr_" [projectname, prid] 
                                                                , jobsetForceeval = Just True
                                                                }

assignNametoInputs :: [Jobsetinput] -> Text -> Text -> [Jobsetinput]
assignNametoInputs jobsetinputs projectname prid =   (\jobsetinput -> jobsetinput { jobsetinputJobset = intercalate "_pr_" [projectname, prid] } ) <$> jobsetinputs

assignNametoInputalts :: [Jobsetinputalt] -> Text -> Text -> [Jobsetinputalt]
assignNametoInputalts jobsetinputalts projectname prid = (\jobsetinputalt -> jobsetinputalt { jobsetinputaltJobset = intercalate "_pr_" [projectname, prid] } ) <$> jobsetinputalts

                        