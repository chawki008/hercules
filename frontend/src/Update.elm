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
                            , Http.send GetProjects  (H.getProjects "/api")  ]    
                
                NewProject -> 
                    (model, Navigation.newUrl (pageToURL page))

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
                _ -> 
                    ( model, Cmd.none)
        UpdateNewProject field value -> 
            ({ model | newProjectPage = updateNewProjectPage model.newProjectPage field value} , Cmd.none)
        
        AddProjectSubmit -> 
            (model, Http.send AddProject (H.postProjects "/api"  (U.unMapProject (Maybe.withDefault emptyProject model.newProjectPage.project))))
        
        AddProject (Ok addResult) -> 
                let
                    project = Maybe.withDefault emptyProject model.newProjectPage.project
                in
                    (model, Task.succeed (NewPage (Urls.Project project.id)) |> Task.perform identity)
        AddProject (Err e ) ->
            ( Debug.log (toString e) model, Cmd.none ) 
-- Ports

emptyProject : Project 
emptyProject =  { id = ""
                , name = ""
                , description = ""
                , isShown =  False
                , isEnabled =  False
                , jobsets = []
                , owner = ""
                , url = ""
                }

updateNewProjectPage : NewProjectPage -> String -> String -> NewProjectPage
updateNewProjectPage newProjectPage field value = let 
                                                    mProject = newProjectPage.project
                                                  in 
                                                    case mProject of 
                                                        Just project -> {newProjectPage | project = Just <| updateProject project field value  }
                                                        Nothing -> {newProjectPage | project = Just <| updateProject emptyProject field value }
                    
updateProject : Project -> String -> String -> Project 
updateProject project field value =
              case field of 
                "id" -> {project | id = value}            
                "name" -> {project | name = value}            
                "description" -> {project | description = value}            
                "owner" -> {project | owner = value}       
                "url" -> {project | url = value}     
                _ -> project            

updateNewProjectToggle : NewProjectPage -> Int -> NewProjectPage
updateNewProjectToggle newProjectPage k = let 
                                            updtatedToggleProjectPage = {newProjectPage | toggles = Array.set k (U.get k newProjectPage.toggles |> not) newProjectPage.toggles}
                                            mProject = updtatedToggleProjectPage.project
                                            updatedProject1 project = {project | isEnabled = U.get 1 updtatedToggleProjectPage.toggles }
                                            updatedProject2 project = {project | isShown = U.get 0 updtatedToggleProjectPage.toggles }
                                          in 
                                            case mProject of 
                                                Just project -> {updtatedToggleProjectPage | project = Just (Debug.log "project" (project |> updatedProject2 << updatedProject1))}
                                                Nothing ->      {updtatedToggleProjectPage | project = Just { id = ""
                                                                                                            , name = ""
                                                                                                            , description = ""
                                                                                                            , isShown =  U.get 0 updtatedToggleProjectPage.toggles
                                                                                                            , isEnabled =  U.get 1 updtatedToggleProjectPage.toggles
                                                                                                            , jobsets = []
                                                                                                            , owner = ""      
                                                                                                            , url = ""                                                              
                                                                                                            }
                                                                }

updateJobsetPage : Result (AjaxError String) JobsetPage -> List (H.JobsetevalWithBuilds) -> Result (AjaxError String) JobsetPage
updateJobsetPage okOldJobsetPage jobs = case okOldJobsetPage of
                                        Ok oldJobsetPage -> 
                                            let 
                                                oldJobsetPage1 = { oldJobsetPage | selectedTab = 1}
                                            in
                                                Ok {oldJobsetPage1 | jobs = Debug.log "jobs" (U.mapToJobs jobs)}
                                        _ -> 
                                             Err (AjaxFail "no jobset yet") 

getJobsetProjectFromJobsetpage : Result (AjaxError String) JobsetPage -> (String, String)
getJobsetProjectFromJobsetpage okJobsetPage = case okJobsetPage of 
                                Ok jobsetPage ->
                                    (jobsetPage.name, jobsetPage.project)
                                _ -> 
                                    ("", "")    

port title : String -> Cmd msg
