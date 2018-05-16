module Update exposing (..)

import Material
import Models exposing (..)
import Msg exposing (..)
import Components.LiveSearch as LiveSearch
import Actions.Project as ActProject
import Actions.Navigation as ActNav
import Actions.Login as ActLogin
import Actions.Jobset as ActJobset
import Actions.Jobseteval as ActJobseteval
import Actions.TabAndToggle as ActTabToggle

update : Msg -> AppModel -> ( AppModel, Cmd Msg )
update msg model =
    case msg of

    -- ###### Login Msgs ###### --

        LoginUserClick loginType ->
           ActLogin.loginUserClick model loginType 

        LogoutUserClick ->
            ( { model | user = Nothing }, Cmd.none )

    -- ###### Login Msgs ###### --

    -- ###### Project Msgs ###### --

        GetProjects (Ok json) ->
           ActProject.getProjects model json

        GetProjects (Err e) ->
            ( Debug.log (toString e) model, Cmd.none ) 
        
        GetProjectWithJobsets (Ok project) ->
           ActProject.getProjectWithJobsets model project

        GetProjectWithJobsets (Err e) ->
            ( Debug.log (toString e) model, Cmd.none ) 

        UpdateNewProject field value -> 
            ActProject.updateNewProject model field value 

        AddProjectSubmit -> 
            ActProject.addProjectSubmit model 

        AddProject (Ok addResult) -> 
            ActProject.addProject model addResult

        AddProject (Err e ) ->
            ( Debug.log (toString e) model, Cmd.none ) 
        
    -- ###### Project Msgs ###### --

    -- ###### Jobset Msgs ###### --

        UpdateNewJobset field value -> 
            ActJobset.updateNewJobset model field value 

        UpdateNewJobsetInput field index value -> 
            ActJobset.updateNewJobsetInput model field index value 

        AddJobsetInput ->
            ActJobset.addJobsetInputM model 

        AddJobsetSubmit -> 
            ActJobset.addJobsetSubmit model
        
        AddJobset (Ok addResult) -> 
                ActJobset.addJobset model addResult

        AddJobset (Err e ) ->
            ( Debug.log (toString e) model, Cmd.none ) 

    -- ###### Jobset Msgs ###### --


    -- ###### Nav Msgs ###### --

        NewPage page ->
            ActNav.route model page  

        UrlChange location ->
                ActNav.urlChange model location  
                
    -- ###### Nav Msgs ###### --

    -- ###### Jobseteval Msgs ###### --
     
        GetJobsetEvals (Ok evalList) ->
                ActJobseteval.getJobsetEvals model evalList

        GetJobsetEvals (Err e) ->
            ( Debug.log (toString e) model, Cmd.none ) 
        
        GetJobsInfo (Ok jobsInfo) -> 
            ActJobseteval.getJobsInfo model jobsInfo 

        GetJobsInfo (Err e ) ->
            ( Debug.log (toString e) model, Cmd.none ) 

    -- ###### Jobseteval Msgs ###### --
    
    -- ###### Tabs and Toggles Msgs ###### --

        SelectTab tabNum ->
            ActTabToggle.selectTab model tabNum 

        SelectLayoutTab tabNum ->
            ActTabToggle.selectLayoutTab model tabNum  

        SwitchToggle page k  -> 
            ActTabToggle.switchToggle model page k 
                 

    -- ###### Tabs and Toggles Msgs ###### --

        Mdl msg_ ->
            Material.update msg_ model

        FetchSucceed init ->
            ( model, Cmd.none )

        FetchFail msg ->
            ( model, Cmd.none )

        PreferencesClick ->
            ( model, Cmd.none )

        LiveSearchMsg searchmsg ->
            let
                ( newmodel, cmds ) =
                    LiveSearch.update searchmsg model
            in
                ( newmodel, Cmd.map LiveSearchMsg cmds )

        GetQueueSummary (Ok queueSummary) ->
                 let
                     oldQueueStats = model.queueStats
                     newQueueStats = {oldQueueStats | numWaiting = queueSummary.queueSummaryAll }
                     newModel = {model | queueStats = newQueueStats}
                 in
                    ({newModel | queueSummary = queueSummary }, Cmd.none)
        
        GetQueueSummary (Err e) -> 
                ( Debug.log (toString e) model, Cmd.none ) 

        Test str -> ({ model | backendURL = Debug.log "t"  str}, Cmd.none)


