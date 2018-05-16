module Actions.TabAndToggle exposing (..)

import Utils as U
import Models exposing (..)
import Msg exposing (..)
import Hercules as H
import Array
import Http
import Navigation
import Urls exposing (..)

-- ####################################################### --
selectTab : AppModel -> Int -> (AppModel, Cmd Msg)
selectTab model tabNum = 
        let 
            okOldJobsetPage = model.jobsetPage 
            jobsetProject = getJobsetProjectFromJobsetpage okOldJobsetPage
            jobset = Tuple.first jobsetProject 
            project = Tuple.second jobsetProject
        in     
            case (Debug.log "tab" tabNum) of 
                1 -> 
                    (model, Http.send GetJobsInfo  (H.getProjectsByProjectNameByJobsetNameJobs "/api" project jobset))
                _ -> 
                    (model, Http.send GetJobsetEvals  (H.getProjectsByProjectNameByJobsetNameJobsetevals "/api" project jobset))

-- ####################################################### --
selectLayoutTab : AppModel -> Int -> (AppModel, Cmd Msg)
selectLayoutTab model tabNum = 
        case tabNum of 
            1 ->  model ! [Navigation.newUrl ( Debug.log "sup" pageToURL QueueSummary)
                            , Http.send GetQueueSummary (H.getQueue_summary "/api")]

            _ -> (model, Cmd.none)
    
-- ####################################################### --
switchToggle : AppModel -> Page -> Int -> (AppModel, Cmd Msg)
switchToggle model page k = 
        case page of 
            NewProject -> 
                ({model | newProjectPage = updateNewProjectToggle model.newProjectPage k}, Cmd.none)
            NewJobset _ -> 
                ({model | newJobsetPage = updateNewJobsetToggle model.newJobsetPage k}, Cmd.none)
            _ -> 
                ( model, Cmd.none)
       
getJobsetProjectFromJobsetpage : Result (AjaxError String) JobsetPage -> (String, String)
getJobsetProjectFromJobsetpage okJobsetPage = case okJobsetPage of 
                                Ok jobsetPage ->
                                    (jobsetPage.name, jobsetPage.project)
                                _ -> 
                                    ("", "")    

updateNewProjectToggle : NewProjectPage -> Int -> NewProjectPage
updateNewProjectToggle newProjectPage k = let 
                                            updatedToggleProjectPage = {newProjectPage | toggles = Array.set k (U.get k newProjectPage.toggles |> not) newProjectPage.toggles}
                                            mProject = updatedToggleProjectPage.project
                                            updEnab project = {project | isEnabled = U.get 1 updatedToggleProjectPage.toggles }
                                            updShown project = {project | isShown = U.get 0 updatedToggleProjectPage.toggles }
                                          in 
                                            case mProject of 
                                                Just project -> {updatedToggleProjectPage | project = Just (project |> updShown << updEnab)}
                                                Nothing->      {updatedToggleProjectPage | project = Just (U.emptyProject |> updShown << updEnab)}
                                                                
updateNewJobsetToggle : NewJobsetPage -> Int -> NewJobsetPage
updateNewJobsetToggle newJobsetPage k = let 
                                            updatedToggleJobsetPage = updateToggleJobsetPage newJobsetPage k 
                                            mJobsetWithInputs = updatedToggleJobsetPage.jobset
                                            updEnab jobsetWithInputs = if k >= 0 && k <= 2 then  {jobsetWithInputs | enabled = Debug.log "k: " k} else jobsetWithInputs
                                            updVisi jobsetWithInputs = { jobsetWithInputs | hidden = U.get 3 updatedToggleJobsetPage.toggles |> not }
                                            updEnabEmail jobsetWithInputs = { jobsetWithInputs | enableemail = U.get 4 updatedToggleJobsetPage.toggles }
                                          in 
                                            case mJobsetWithInputs of 
                                                Just jobsetWithInputs -> {updatedToggleJobsetPage | jobset =  Just (jobsetWithInputs |> updEnab << updVisi << updEnabEmail)}
                                                Nothing -> {updatedToggleJobsetPage | jobset = Just (U.emptyJobsetWithInputs |> updEnab << updVisi << updEnabEmail)}
                                                                
                                                                
updateToggleJobsetPage : NewJobsetPage -> Int -> NewJobsetPage
updateToggleJobsetPage  newJobsetPage k = 
                            let 
                                radioTreatement k = {newJobsetPage | toggles = updateRadio k ((k+1) % 3) ((k+2) % 3) newJobsetPage.toggles}
                            in
                                case k of 
                                    0 -> radioTreatement 2 
                                    1 -> radioTreatement 0
                                    2 -> radioTreatement 1
                                    k -> {newJobsetPage | toggles = Array.set k (U.get k newJobsetPage.toggles |> not) newJobsetPage.toggles}


updateRadio :Int -> Int -> Int -> Array.Array Bool -> Array.Array Bool
updateRadio k0 k1 k2 toggles = Array.set k1 False (Array.set k2 False (Array.set k0 True toggles))


