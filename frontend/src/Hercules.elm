module Hercules exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Http
import String
import Models as M
import Date

type alias Project =
    { projectName : String
    , projectDisplayname : String
    , projectDescription : Maybe (String)
    , projectEnabled : Int
    , projectHidden : Int
    , projectOwner : String
    , projectHomepage : Maybe (String)
    }

decodeProject : Decoder Project
decodeProject =
    decode Project
        |> required "projectName" string
        |> required "projectDisplayname" string
        |> required "projectDescription" (maybe string)
        |> required "projectEnabled" int
        |> required "projectHidden" int
        |> required "projectOwner" string
        |> required "projectHomepage" (maybe string)

type alias Jobset =
    { jobsetName : String
    , jobsetProject : String
    , jobsetDescription : Maybe (String)
    , jobsetNixexprinput : String
    , jobsetNixexprpath : String
    , jobsetErrormsg : Maybe (String)
    , jobsetErrortime : Maybe (Int)
    , jobsetLastcheckedtime : Maybe (Int)
    , jobsetTriggertime : Maybe (Int)
    , jobsetEnabled : Int
    , jobsetEnableemail : Int
    , jobsetHidden : Int
    , jobsetEmailoverride : String
    , jobsetKeepnr : Int
    , jobsetCheckinterval : Int
    , jobsetSchedulingshares : Int
    , jobsetFetcherrormsg : Maybe (String)
    }

decodeJobset : Decoder Jobset
decodeJobset =
    decode Jobset
        |> required "jobsetName" string
        |> required "jobsetProject" string
        |> required "jobsetDescription" (maybe string)
        |> required "jobsetNixexprinput" string
        |> required "jobsetNixexprpath" string
        |> required "jobsetErrormsg" (maybe string)
        |> required "jobsetErrortime" (maybe int)
        |> required "jobsetLastcheckedtime" (maybe int)
        |> required "jobsetTriggertime" (maybe int)
        |> required "jobsetEnabled" int
        |> required "jobsetEnableemail" int
        |> required "jobsetHidden" int
        |> required "jobsetEmailoverride" string
        |> required "jobsetKeepnr" int
        |> required "jobsetCheckinterval" int
        |> required "jobsetSchedulingshares" int
        |> required "jobsetFetcherrormsg" (maybe string)

type alias ProjectWithJobsets =
    { project : Project
    , jobsets : List (JobsetWithStatus)
    }

decodeProjectWithJobsets : Decoder ProjectWithJobsets
decodeProjectWithJobsets =
    decode ProjectWithJobsets
        |> required "project" decodeProject
        |> required "jobsets" (list decodeJobsetWithStatus)

type alias JobsetStatus = 
    { succeeded : Maybe Int
    , failed : Maybe Int
    , queued : Maybe Int
    , lastevaluatedAt : Int   
    }

decodeJobsetStatus : Decoder JobsetStatus
decodeJobsetStatus = 
    decode JobsetStatus
        |> required "succeed" (maybe int)
        |> required "failed" (maybe int)
        |> required "queued" (maybe int)
        |> required "lastevaluatedAt" int

type alias JobsetWithStatus =
    { jobset : Jobset
    , jobsetStatus : JobsetStatus
    }

    
decodeJobsetWithStatus : Decoder JobsetWithStatus
decodeJobsetWithStatus = 
    decode JobsetWithStatus
        |> required "jobset" decodeJobset
        |> required "jobsetStatus" decodeJobsetStatus

type alias Jobseteval =
    { jobsetevalId : Int
    , jobsetevalProject : String
    , jobsetevalJobset :  String
    , jobsetevalTimestamp : Int
    , jobsetevalCheckouttime : Int
    , jobsetevalEvaltime : Int
    , jobsetevalHasnewbuilds : Int
    , jobsetevalHash : String
    , jobsetevalNrbuilds : Maybe (Int)
    , jobsetevalNrsucceeded : Maybe (Int)
    }

decodeJobseteval : Decoder Jobseteval
decodeJobseteval =
    decode Jobseteval
        |> required "jobsetevalId" int
        |> required "jobsetevalProject" string
        |> required "jobsetevalJobset"  string
        |> required "jobsetevalTimestamp" int
        |> required "jobsetevalCheckouttime" int
        |> required "jobsetevalEvaltime"  int
        |> required "jobsetevalHasnewbuilds"  int
        |> required "jobsetevalHash"  string
        |> required "jobsetevalNrbuilds"  (maybe int)
        |> required "jobsetevalNrsucceeded"  (maybe int)

getProjectNames : String -> Http.Request (List (String))
getProjectNames urlBase =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ urlBase
                , "projectNames"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list string)
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getProjects : String -> Http.Request (List (Project))
getProjects urlBase =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ urlBase
                , "project"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list decodeProject)
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getProjectByProjectName : String -> String -> Http.Request (Maybe (Project))
getProjectByProjectName urlBase capture_projectName =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ urlBase
                , "project"
                , capture_projectName |> Http.encodeUri
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (maybe decodeProject)
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getProjectWithJobsets : String -> Http.Request (List (ProjectWithJobsets))
getProjectWithJobsets urlBase =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ urlBase
                , "projectWithJobsets"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list decodeProjectWithJobsets)
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getJobsetEvals : String -> String -> String -> Http.Request (List (Jobseteval))
getJobsetEvals urlBase projectId jobsetName =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ urlBase
                , "project"
                , projectId |> Http.encodeUri
                , jobsetName |> Http.encodeUri
                , "jobsetevals"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list decodeJobseteval)
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getProtected : String -> String -> Http.Request (String)
getProtected urlBase header_authorization =
    Http.request
        { method =
            "GET"
        , headers =
            [ Http.header "authorization" header_authorization
            ]
        , url =
            String.join "/"
                [ urlBase
                , "protected"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson string
        , timeout =
            Nothing
        , withCredentials =
            False
        }


mapProjectWithJobsets : ProjectWithJobsets -> M.Project 
mapProjectWithJobsets projectWithJobsets = 
              { id = projectWithJobsets.project.projectName
              , name = projectWithJobsets.project.projectDisplayname
              , description = Maybe.withDefault "" projectWithJobsets.project.projectDescription 
              , isShown = True
              , jobsets = List.map mapJobset projectWithJobsets.jobsets
              }

mapProject : Project -> M.Project
mapProject project = 
              { id = project.projectName
              , name = project.projectDisplayname
              , description = Maybe.withDefault "" project.projectDescription 
              , isShown = True
              , jobsets = []
              }


mapJobset : JobsetWithStatus -> M.Jobset
mapJobset jobsetWithStatus = 
                     let 
                         lastevaluated = Date.fromTime (toFloat (jobsetWithStatus.jobsetStatus.lastevaluatedAt*1000))
                         year = toString (Date.year lastevaluated)
                         month = toString (Date.month lastevaluated)
                         day = toString (Date.day lastevaluated)
                         hour = toString (Date.hour lastevaluated)
                         minute = toString (Date.minute lastevaluated)
                     in 
                      { id = jobsetWithStatus.jobset.jobsetName
                      , name = jobsetWithStatus.jobset.jobsetName
                      , description = Maybe.withDefault ""  jobsetWithStatus.jobset.jobsetDescription
                      , queued = Maybe.withDefault 0 jobsetWithStatus.jobsetStatus.queued
                      , failed = Maybe.withDefault 0 jobsetWithStatus.jobsetStatus.failed
                      , succeeded = Maybe.withDefault 0 jobsetWithStatus.jobsetStatus.succeeded
                      , isShown = True
                      , lastEvaluation =  year ++ "-" ++ month ++ "-" ++ day ++ " " ++ hour ++ ":" ++ minute
                      } 
                       
                      
mapJobsetevalsToJobsetPage : List (Jobseteval) -> M.JobsetPage
mapJobsetevalsToJobsetPage jobsetevals = 
    { latestCheckTime = Date.fromString "2016-08-06 12:38:01"
    , latestEvaluationTime = Date.fromString "2016-08-06 12:38:01"
    , latestFinishedEvaluationTime = Date.fromString "2016-08-06 12:38:01"
    , evaluations = List.map mapJobsetevalToEval jobsetevals
    , name = "s"
    }

mapJobsetevalToEval : Jobseteval -> M.Evaluation
mapJobsetevalToEval jobseteval = 
    { id = jobseteval.jobsetevalId
    , inputChanges = "dsds"
    , jobSummary = { succeeded = Maybe.withDefault 0 jobseteval.jobsetevalNrsucceeded 
                   , failed = (Maybe.withDefault 0 jobseteval.jobsetevalNrbuilds) - (Maybe.withDefault 0 jobseteval.jobsetevalNrsucceeded) 
                   , inQueue  =  Maybe.withDefault 0 jobseteval.jobsetevalNrsucceeded 
                   }
    , evaluatedAt = Date.fromString (toString jobseteval.jobsetevalEvaltime)
    }    