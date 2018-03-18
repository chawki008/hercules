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
                Project projectId  -> 
                    model ! [Navigation.newUrl (pageToURL page)
                            ,Http.send GetProjectWithJobsets  (H.getProjectWithJobsets "/api" projectId ) ] 

                _ ->    
                    ( model, Navigation.newUrl (pageToURL page) )

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
            ( { model | projects = List.map H.mapProject json }, Cmd.none )

        GetProjects (Err e) ->
            ( Debug.log (toString e) model, Cmd.none ) 
        
        GetProjectWithJobsets (Ok json) ->
            let 
                oldProjects = model.projects
                newProject = H.mapProjectWithJobsets json
                updateProject : Project -> Project
                updateProject project = if project.id == newProject.id then newProject else project
                newProjects = List.map updateProject oldProjects 
            in
                ( { model | projects = newProjects }, Cmd.none )

        GetProjectWithJobsets (Err e) ->
            ( Debug.log (toString e) model, Cmd.none ) 

-- Ports


port title : String -> Cmd msg
