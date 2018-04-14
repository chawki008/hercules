
module Hercules.Helpers.JSONParsers where 



import           Hercules.Database.Extra 
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.HashMap.Strict  as M
import 			 Data.Int 			(Int32)
import 			 Data.Text


instance FromJSON JobsetWithInputs where
  parseJSON j = do
			jobsetWithInputs 		 <- parseJSON j 
			jobset					 <- getJobset jobsetWithInputs
			jsonInputs 				 <- jobsetWithInputs .: (pack "inputs")	
			inputs 			 		 <- getInputsFromJSON (jobsetProject jobset) (jobsetName jobset) jsonInputs
			return JobsetWithInputs { jobsetWithInputsJobset = jobset
									 , jobsetWithInputsInputs = fst inputs
									 , jobsetWithInputsInputsalt = snd inputs
									 }
									 
				   
getJobset :: Object -> Parser Jobset
getJobset jobsetWithInputs = do 
						jobsetName		 		 <- jobsetWithInputs .: (pack "name")
						jobsetProject	 		 <- jobsetWithInputs .:? (pack "project") .!= (pack "tmpProject") 
						jobsetErrormsg	 		 <- jobsetWithInputs .:? (pack "errormsg") ::  Parser (Maybe Text)
						jobsetErrortime	 		 <- jobsetWithInputs .:? (pack "errortime") :: Parser (Maybe Int32)
						jobsetLastcheckedtime	 <- jobsetWithInputs .:? (pack "lastcheckedtime") :: Parser (Maybe Int32)
						jobsetTriggertime    	 <- jobsetWithInputs .:? (pack "triggertime") :: Parser (Maybe Int32)
						jobsetFetcherrormsg    	 <- jobsetWithInputs .:? (pack "fetcherrormsg") :: Parser (Maybe Text)
						jobsetEnabled	 		 <- jobsetWithInputs .: ((pack "enabled")) :: Parser Int32
						jobsetHidden	 		 <- jobsetWithInputs .: ((pack "hidden")) :: Parser Bool
						jobsetDescription		 <- jobsetWithInputs .:? ((pack "description")) :: Parser (Maybe Text)
						jobsetNixexprinput		 <- jobsetWithInputs .: ((pack "nixexprinput")) :: Parser Text
						jobsetCheckinterval		 <- jobsetWithInputs .: ((pack "checkinterval")) :: Parser Int32
						jobsetSchedulingshares	 <- jobsetWithInputs .: ((pack "schedulingshares")) :: Parser Int32
						jobsetEnableemail		 <- jobsetWithInputs .: ((pack "enableemail")) :: Parser Bool
						jobsetEmailoverride		 <- jobsetWithInputs .: ((pack "emailoverride")) :: Parser Text
						jobsetNixexprpath		 <- jobsetWithInputs .: ((pack "nixexprpath")) :: Parser Text
						jobsetKeepnr		 	 <- jobsetWithInputs .: ((pack "keepnr")) :: Parser Int32
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
									  }

getInputsFromJSON ::   Text -> Text -> Value -> Parser ([Jobsetinput], [Jobsetinputalt])
getInputsFromJSON projectName jobsetName = withObject "set of inputs" $ \inputsObject -> do
																			inputsNames <- return (M.keys (inputsObject :: Object)) 
																			inputsAndInputsalts <- mapM (getInput inputsObject projectName jobsetName) inputsNames
																			return (Prelude.unzip inputsAndInputsalts)
		
getInput :: Object -> Text -> Text -> Text -> Parser (Jobsetinput, Jobsetinputalt)
getInput inputsObject projectName jobsetName inputName = do 
								inputInfos <- inputsObject .: inputName
								inputType  <- inputInfos .: (pack "type") :: Parser Text
								inputEmail <- inputInfos .: (pack "emailresponsible") :: Parser Bool
								inputValue <- inputInfos .:? (pack "value") :: Parser (Maybe Text)
								inputRevision <- inputInfos .:? (pack "revision") :: Parser (Maybe Text)
								return ( Jobsetinput { jobsetinputProject          = projectName
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

