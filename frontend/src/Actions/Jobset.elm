module Actions.Jobset exposing(..)

import Utils as U
import Models exposing (..)
import Msg exposing (..)
import Hercules as H
import Array
import Http
import Task 
import Urls 
import Regex exposing (..)

-- ########################################################### --
updateNewJobset : AppModel -> String -> String -> (AppModel, Cmd Msg)
updateNewJobset model field value = 
        ({ model | newJobsetPage = updateNewJobsetPage model.newJobsetPage field value} , Cmd.none)


-- ########################################################### --
updateNewJobsetInput : AppModel -> String -> Int -> String -> (AppModel, Cmd Msg)  
updateNewJobsetInput model field index value = 
        ({model | newJobsetPage = updateNewJobsetPageInput model.newJobsetPage field value index}, Cmd.none)

-- ########################################################### --
addJobsetInputM : AppModel -> (AppModel, Cmd Msg)
addJobsetInputM model = 
        ({model | newJobsetPage = addJobsetInput model.newJobsetPage}, Cmd.none)
        
-- ########################################################### --
addJobsetSubmit : AppModel -> (AppModel, Cmd Msg)
addJobsetSubmit model = 
        (model, Http.send AddJobset (U.postJobset "/api" "model.user.token" model.newJobsetPage.project (Maybe.withDefault U.emptyJobsetWithInputs model.newJobsetPage.jobset)))

-- ########################################################### --
addJobset : AppModel -> String -> (AppModel, Cmd Msg)
addJobset model addResult = 
        let
            project = model.newJobsetPage.project
        in
            (model, Task.succeed (NewPage (Urls.Project project)) |> Task.perform identity)

updateNewJobsetPage : NewJobsetPage -> String -> String -> NewJobsetPage
updateNewJobsetPage newJobsetPage field value = let 
                                                    mJobset = newJobsetPage.jobset
                                                    validationResult =  validateJobsetField field value
                                                in 
                                                    case mJobset of 
                                                        Just jobsetWithInputs -> let 
                                                                                   updatedNewJobset = {newJobsetPage | jobset = Just <| updateJobset jobsetWithInputs field value  }
                                                                                 in 
                                                                                   {updatedNewJobset | validations = U.set 0 validationResult updatedNewJobset.validations}
                                                        Nothing -> {newJobsetPage | jobset = Just <| updateJobset U.emptyJobsetWithInputs field value }


updateJobset : JobsetWithInputs -> String -> String -> JobsetWithInputs 
updateJobset jobsetWithInputs field value =
              case field of 
                "description"       -> {jobsetWithInputs | description = value}            
                "name"              -> {jobsetWithInputs | name = value}            
                "nixexprinput"      -> {jobsetWithInputs | nixexprinput = value}            
                "nixexprpath"       -> {jobsetWithInputs | nixexprpath = value}       
                "emailoverride"     -> {jobsetWithInputs | emailoverride = value}   
                "checkinterval"     -> {jobsetWithInputs | checkinterval = Maybe.withDefault 0 <|Result.toMaybe <| String.toInt value  }  
                "schedulingshares"  -> {jobsetWithInputs | schedulingshares = Maybe.withDefault 0 <|Result.toMaybe <| String.toInt value  }  
                "keepnr"            -> {jobsetWithInputs | keepnr = Maybe.withDefault 0 <|Result.toMaybe <| String.toInt value  }  
                _                   -> jobsetWithInputs            

updateNewJobsetPageInput : NewJobsetPage -> String -> String -> Int -> NewJobsetPage
updateNewJobsetPageInput newJobsetPage field value index = let
                                                             mJobset = newJobsetPage.jobset 
                                                           in
                                                              case mJobset of 
                                                                Just jobsetWithInputs -> {newJobsetPage | jobset = Debug.log "" <| Just <| updateJobsetInput jobsetWithInputs field value index}
                                                                Nothing -> {newJobsetPage | jobset = Just <| updateJobsetInput U.emptyJobsetWithInputs field value index}
                          

updateJobsetInput : JobsetWithInputs -> String -> String -> Int -> JobsetWithInputs 
updateJobsetInput jobsetWithInputs field value index = let 
                                                        input = updateInput (U.getJobsetInput index jobsetWithInputs.inputs) field value
                                                       in
                                                        {jobsetWithInputs | inputs = Array.set index input jobsetWithInputs.inputs}

updateInput : JobsetInput -> String -> String -> JobsetInput 
updateInput input field value = case field of 
                             "inputname" -> {input | inputname = value}  
                             "inputType" -> {input | inputType = "git"}  
                             "value"     -> {input | value = value}  
                             _           -> input

addJobsetInput : NewJobsetPage -> NewJobsetPage
addJobsetInput newJobsetPage = let 
                                 mJobset = newJobsetPage.jobset  
                                 newNewJobsetPage = {newJobsetPage | jobsetInputsNr = newJobsetPage.jobsetInputsNr + 1}  
                               in
                                 case mJobset of 
                                    Just jobsetWithInputs -> 
                                        {newNewJobsetPage | jobset =  Just <| addInput jobsetWithInputs }
                                    Nothing -> {newNewJobsetPage | jobset = Just <| addInput U.emptyJobsetWithInputs }     

addInput : JobsetWithInputs -> JobsetWithInputs 
addInput jobsetWithInputs = {jobsetWithInputs | inputs = Array.push U.emptyJobsetInput jobsetWithInputs.inputs}

validateJobsetField : String -> String -> Bool
validateJobsetField field value =
              case field of 
                "name" -> validateJobsetName value
                _    -> True

validateJobsetName : String -> Bool 
validateJobsetName jobsetName = not <| contains jobsetNameRegex jobsetName 

jobsetNameRegex : Regex 
jobsetNameRegex = regex "^(?:[A-Za-z_][A-Za-z0-9-_/.]*)$"

