module Actions.Project exposing (..)

import Utils as U
import Models exposing (..)
import Msg exposing (..)
import Hercules as H
import Http
import Task 
import Urls 

--  ################################################ --           
getProjects : AppModel -> List H.Project -> (AppModel, Cmd Msg)
getProjects model projectList =  ( { model | projects = List.map U.mapProject projectList }, Cmd.none )

--  ################################################ --           
getProjectWithJobsets : AppModel -> Maybe H.ProjectWithJobsetsWithStatus -> (AppModel, Cmd Msg)
getProjectWithJobsets model projectWithJobsetsWithStatus =  
                            let 
                                oldProjects = model.projects
                                newProject = U.mapProjectWithJobsets projectWithJobsetsWithStatus
                                updateProject : Project -> Project
                                updateProject project = if project.id == newProject.id then newProject else project
                                updatedProjects = List.map updateProject oldProjects 
                                newProjects = if List.member newProject updatedProjects then updatedProjects
                                                                                        else List.append [newProject] oldProjects

                            in
                                ( { model | projects = newProjects }, Cmd.none )

--  ################################################ --           
updateNewProject : AppModel -> String -> String -> (AppModel, Cmd Msg)
updateNewProject model field value = ({ model | newProjectPage = updateNewProjectPage model.newProjectPage field value} , Cmd.none)
         
--  ################################################ --           
addProjectSubmit : AppModel -> (AppModel, Cmd Msg)
addProjectSubmit model = 
        let
            mUser = model.user
        in
            case mUser of 
                Nothing -> 
                    (model, Cmd.none)
                Just user -> 
                    (model, Http.send AddProject (H.postProjects "/api" user.token (U.unMapProject (Maybe.withDefault U.emptyProject model.newProjectPage.project))))
        
--  ################################################ --           
addProject : AppModel -> String -> (AppModel, Cmd Msg)
addProject model addResult = 
                let
                    project = Maybe.withDefault U.emptyProject model.newProjectPage.project
                in
                    (model, Task.succeed (NewPage (Urls.Project project.id)) |> Task.perform identity)
        

updateNewProjectPage : NewProjectPage -> String -> String -> NewProjectPage
updateNewProjectPage newProjectPage field value = let 
                                                    mProject = newProjectPage.project
                                                  in 
                                                    case mProject of 
                                                        Just project -> {newProjectPage | project = Just <| updateProject project field value}
                                                        Nothing -> {newProjectPage | project = Just <| updateProject U.emptyProject field value}
                    
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
