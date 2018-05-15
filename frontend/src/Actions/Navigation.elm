port module Actions.Navigation exposing (..)

import Models exposing (..)
import Msg exposing (..)
import Urls exposing (..)
import Hercules as H
import Http
import Navigation
import UrlParser exposing (parsePath)


route : AppModel -> Page -> (AppModel, Cmd Msg)
route model page = 
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
                newNewJobsetPage = {oldNewJobsetPage | project = Debug.log "hey" project}
            in
                ({model | newJobsetPage = newNewJobsetPage}, Navigation.newUrl (pageToURL (NewJobset project)))
        
        QueueSummary -> 
            model ! [Navigation.newUrl ( Debug.log "sup" pageToURL page)
                    , Http.send GetQueueSummary (H.getQueue_summary "/api")]

urlChange : AppModel -> Navigation.Location -> (AppModel, Cmd Msg)
urlChange model location = 
        let
            page = Maybe.withDefault Home (parsePath pageParser location)
        in
            ( { model | currentPage = (Debug.log "current" page) }
            , title (pageToTitle page)
            )

port title : String -> Cmd msg