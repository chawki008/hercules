port module Update exposing (..)

import Material
import Navigation
import Models exposing (..)
import Msg exposing (..)
import Components.LiveSearch as LiveSearch
import Urls exposing (..)
import UrlParser exposing (parsePath)
import Hercules as H
import Http
import Utils as U
import Array
import Task 

update : Msg -> AppModel -> ( AppModel, Cmd Msg )
update msg model =
    case msg of
        Mdl msg_ ->
            Material.update msg_ model

        FetchSucceed init ->
            ( model, Cmd.none )

        FetchFail msg ->
            ( model, Cmd.none )

        LoginUserClick loginType ->
            let
                -- TODO: well, actually do the login proceedure
                user =
                    { id = "domenkozar"
                    , name = "Domen Kožar"
                    , email = "domen@dev.si"
                    , roles = []
                    , recieveEvaluationErrors = False
                    }
            in
                case loginType of
                    Hydra ->
                        ( { model | user = Just user }, Cmd.none )

                    Google ->
                        ( { model | user = Just user }, Cmd.none )

        LogoutUserClick ->
            -- TODO: well, should we cleanup something?
            ( { model | user = Nothing }, Cmd.none )

        PreferencesClick ->
            ( model, Cmd.none )

        LiveSearchMsg searchmsg ->
            let
                ( newmodel, cmds ) =
                    LiveSearch.update searchmsg model
            in
                ( newmodel, Cmd.map LiveSearchMsg cmds )

        NewPage page ->
            case page of 
                Jobset projectId jobsetName -> 
                    {model | jobsetPage = Err (AjaxFail "no jobsets yet") } ! [Navigation.newUrl (pageToURL page)
                            , Http.send GetJobsetEvals  (H.getProjectsByProjectNameByJobsetNameJobsetevals "/api" projectId jobsetName ) ] 

                Project projectId  -> 
                    model ! [Navigation.newUrl (pageToURL page)
                            ,Http.send GetProjectWithJobsets  (H.getProjectsWithJobsetsByProjectId "/api" projectId ) ] 

                Home ->
                    model ! [Navigation.newUrl (pageToURL page) 
                            , Http.send GetProjects  (H.getProjects "/api") 
                            , Http.send GetQueueSummary (H.getQueue_summary "/api")]    
                
                NewProject -> 
                    (model, Navigation.newUrl (pageToURL page))
                
                NewJobset project ->
                    let 
                        oldNewJobsetPage = model.newJobsetPage
                        newNewJobsetPage = {oldNewJobsetPage | project = project}
                    in
                        ({model | newJobsetPage = newNewJobsetPage}, Navigation.newUrl (pageToURL (NewJobset project)))
                
                QueueSummary -> 
                    model ! [Navigation.newUrl ( Debug.log "sup" pageToURL page)
                            , Http.send GetQueueSummary (H.getQueue_summary "/api")]
                
        GetQueueSummary (Ok queueSummary) ->
                 let
                     oldQueueStats = model.queueStats
                     newQueueStats = {oldQueueStats | numWaiting = queueSummary.queueSummaryAll }
                     newModel = {model | queueStats = newQueueStats}
                 in
                    ({newModel | queueSummary = queueSummary }, Cmd.none)
        
        GetQueueSummary (Err e) -> 
                ( Debug.log (toString e) model, Cmd.none ) 

        ClickCreateProject ->
            -- TODO: http
            ( model, Cmd.none )

        UrlChange location ->
            let
                page = Maybe.withDefault Home (parsePath pageParser location)
            in
            ( { model | currentPage = page }
            , title (pageToTitle page)
            )
            
        GetProjects (Ok json) ->
            ( { model | projects = List.map U.mapProject json }, Cmd.none )

        GetProjects (Err e) ->
            ( Debug.log (toString e) model, Cmd.none ) 
        
        GetProjectWithJobsets (Ok project) ->
            let 
                oldProjects = model.projects
                newProject = U.mapProjectWithJobsets project
                updateProject : Project -> Project
                updateProject project = if project.id == newProject.id then newProject else project
                updatedProjects = List.map updateProject oldProjects 
                newProjects = if List.member newProject updatedProjects then updatedProjects
                                                                         else List.append [newProject] oldProjects

            in
                ( { model | projects = newProjects }, Cmd.none )

        GetProjectWithJobsets (Err e) ->
            ( Debug.log (toString e) model, Cmd.none ) 

        GetJobsetEvals (Ok json) ->
            ({ model | jobsetPage = Ok (U.mapJobsetevalsToJobsetPage json) }, Cmd.none )

        GetJobsetEvals (Err e) ->
            ( Debug.log (toString e) model, Cmd.none ) 

        SelectTab tabNum ->
            let 
                okOldJobsetPage = model.jobsetPage 
                jobsetProject = getJobsetProjectFromJobsetpage okOldJobsetPage
                jobset = Tuple.first jobsetProject 
                project = Tuple.second jobsetProject
            in     
                case tabNum of 
                    1 -> 
                        (model, Http.send GetJobsInfo  (H.getProjectsByProjectNameByJobsetNameJobs "/api" project jobset))
                    _ -> 
                        (model, Http.send GetJobsetEvals  (H.getProjectsByProjectNameByJobsetNameJobsetevals "/api" project jobset))

        SelectLayoutTab tabNum ->
            case tabNum of 
                1 ->  model ! [Navigation.newUrl ( Debug.log "sup" pageToURL QueueSummary)
                              , Http.send GetQueueSummary (H.getQueue_summary "/api")]

                _ -> (model, Cmd.none)

        GetJobsInfo (Ok jobsInfo) -> 
            let 
                okOldJobsetPage = model.jobsetPage 
                newJobsetPage = updateJobsetPage okOldJobsetPage jobsInfo

            in
                ({model | jobsetPage = newJobsetPage }, Cmd.none )
       
        GetJobsInfo (Err e ) ->
            ( Debug.log (toString e) model, Cmd.none ) 

        SwitchToggle page k  -> 
            case page of 
                NewProject -> 
                    ({model | newProjectPage = updateNewProjectToggle model.newProjectPage k}, Cmd.none)
                NewJobset _ -> 
                    ({model | newJobsetPage = updateNewJobsetToggle model.newJobsetPage k}, Cmd.none)
                _ -> 
                    ( model, Cmd.none)
                    
        UpdateNewProject field value -> 
            ({ model | newProjectPage = updateNewProjectPage model.newProjectPage field value} , Cmd.none)
        
        AddProjectSubmit -> 
            (model, Http.send AddProject (H.postProjects "/api"  (U.unMapProject (Maybe.withDefault U.emptyProject model.newProjectPage.project))))
        
        AddProject (Ok addResult) -> 
                let
                    project = Maybe.withDefault U.emptyProject model.newProjectPage.project
                in
                    (model, Task.succeed (NewPage (Urls.Project project.id)) |> Task.perform identity)

        AddProject (Err e ) ->
            ( Debug.log (toString e) model, Cmd.none ) 
        
        UpdateNewJobset field value -> 
            ({ model | newJobsetPage = updateNewJobsetPage model.newJobsetPage field value} , Cmd.none)

        UpdateNewJobsetInput field index value -> 
            ({model | newJobsetPage = updateNewJobsetPageInput model.newJobsetPage field value index}, Cmd.none)
        
        AddJobsetInput ->
            ({model | newJobsetPage = addJobsetInput model.newJobsetPage}, Cmd.none)
        
        AddJobsetSubmit -> 
            (model, Http.send AddJobset (U.postJobset "/api" model.newJobsetPage.project (Maybe.withDefault U.emptyJobsetWithInputs model.newJobsetPage.jobset)))
        
        AddJobset (Ok addResult) -> 
                let
                    project = model.newJobsetPage.project
                in
                    (model, Task.succeed (NewPage (Urls.Project project)) |> Task.perform identity)

        AddJobset (Err e ) ->
            ( Debug.log (toString e) model, Cmd.none ) 
        Test str -> ({ model | backendURL = Debug.log "t"  str}, Cmd.none)

        -- NewJobset projectIndex -> model ! [Navigation.newUrl (page)] 
-- Ports

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
                             "inputType" -> {input | inputType = value}  
                             "value"     -> {input | value = value}  
                             _           -> input

updateNewProjectPage : NewProjectPage -> String -> String -> NewProjectPage
updateNewProjectPage newProjectPage field value = let 
                                                    mProject = newProjectPage.project
                                                  in 
                                                    case mProject of 
                                                        Just project -> {newProjectPage | project = Just <| updateProject project field value}
                                                        Nothing -> {newProjectPage | project = Just <| updateProject U.emptyProject field value}

updateNewJobsetPage : NewJobsetPage -> String -> String -> NewJobsetPage
updateNewJobsetPage newJobsetPage field value = let 
                                                    mJobset = newJobsetPage.jobset
                                                in 
                                                    case mJobset of 
                                                        Just jobsetWithInputs -> {newJobsetPage | jobset = Debug.log "" <| Just <| updateJobset jobsetWithInputs field value  }
                                                        Nothing -> {newJobsetPage | jobset = Just <| updateJobset U.emptyJobsetWithInputs field value }
                    
updateProject : Project -> String -> String -> Project 
updateProject project field value =
              case field of 
                "id"          -> {project | id = value}            
                "name"        -> {project | name = value}            
                "description" -> {project | description = value}            
                "owner"       -> {project | owner = value}       
                "url"         -> {project | url = value}     
                "repo"        -> {project | repo = value}     
                _             -> project     

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
                                            updEnab jobsetWithInputs = if k >= 0 && k <= 2 then  {jobsetWithInputs | enabled = k} else jobsetWithInputs
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
                                    0 -> radioTreatement 0 
                                    1 -> radioTreatement 1
                                    2 -> radioTreatement 2
                                    k -> {newJobsetPage | toggles = Array.set k (U.get k newJobsetPage.toggles |> not) newJobsetPage.toggles}


updateRadio :Int -> Int -> Int -> Array.Array Bool -> Array.Array Bool
updateRadio k0 k1 k2 toggles = Array.set k1 False (Array.set k2 False (Array.set k0 True toggles))

updateJobsetPage : Result (AjaxError String) JobsetPage -> List (H.JobsetevalWithBuilds) -> Result (AjaxError String) JobsetPage
updateJobsetPage okOldJobsetPage jobs = case okOldJobsetPage of
                                        Ok oldJobsetPage -> 
                                            let 
                                                oldJobsetPage1 = { oldJobsetPage | selectedTab = 1}
                                            in
                                                Ok {oldJobsetPage1 | jobs = U.mapToJobs jobs}
                                        _ -> 
                                             Err (AjaxFail "no jobset yet") 

getJobsetProjectFromJobsetpage : Result (AjaxError String) JobsetPage -> (String, String)
getJobsetProjectFromJobsetpage okJobsetPage = case okJobsetPage of 
                                Ok jobsetPage ->
                                    (jobsetPage.name, jobsetPage.project)
                                _ -> 
                                    ("", "")    

port title : String -> Cmd msg
