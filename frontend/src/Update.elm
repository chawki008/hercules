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
                _ ->    
                    ( model, Navigation.newUrl (pageToURL Home) )

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

-- Ports


port title : String -> Cmd msg
