module Hercules exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Http
import String


type alias Project =
    { projectName : String
    , projectDisplayname : String
    , projectDescription : Maybe (String)
    , projectEnabled : Int
    , projectHidden : Int
    , projectOwner : String
    , projectHomepage : Maybe (String)
    , projectRepo : Maybe (String)
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
        |> required "projectRepo" (maybe string)

encodeProject : Project -> Json.Encode.Value
encodeProject x =
    Json.Encode.object
        [ ( "projectName", Json.Encode.string x.projectName )
        , ( "projectDisplayname", Json.Encode.string x.projectDisplayname )
        , ( "projectDescription", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.string) x.projectDescription )
        , ( "projectEnabled", Json.Encode.int x.projectEnabled )
        , ( "projectHidden", Json.Encode.int x.projectHidden )
        , ( "projectOwner", Json.Encode.string x.projectOwner )
        , ( "projectHomepage", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.string) x.projectHomepage )
        , ( "projectRepo", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.string) x.projectRepo )
        ]

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
    , jobsetForceeval : Maybe (Bool)
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
        |> required "jobsetForceeval" (maybe bool)

encodeJobset : Jobset -> Json.Encode.Value
encodeJobset x =
    Json.Encode.object
        [ ( "jobsetName", Json.Encode.string x.jobsetName )
        , ( "jobsetProject", Json.Encode.string x.jobsetProject )
        , ( "jobsetDescription", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.string) x.jobsetDescription )
        , ( "jobsetNixexprinput", Json.Encode.string x.jobsetNixexprinput )
        , ( "jobsetNixexprpath", Json.Encode.string x.jobsetNixexprpath )
        , ( "jobsetErrormsg", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.string) x.jobsetErrormsg )
        , ( "jobsetErrortime", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.int) x.jobsetErrortime )
        , ( "jobsetLastcheckedtime", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.int) x.jobsetLastcheckedtime )
        , ( "jobsetTriggertime", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.int) x.jobsetTriggertime )
        , ( "jobsetEnabled", Json.Encode.int x.jobsetEnabled )
        , ( "jobsetEnableemail", Json.Encode.int x.jobsetEnableemail )
        , ( "jobsetHidden", Json.Encode.int x.jobsetHidden )
        , ( "jobsetEmailoverride", Json.Encode.string x.jobsetEmailoverride )
        , ( "jobsetKeepnr", Json.Encode.int x.jobsetKeepnr )
        , ( "jobsetCheckinterval", Json.Encode.int x.jobsetCheckinterval )
        , ( "jobsetSchedulingshares", Json.Encode.int x.jobsetSchedulingshares )
        , ( "jobsetFetcherrormsg", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.string) x.jobsetFetcherrormsg )
        , ( "jobsetForceeval", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.bool) x.jobsetForceeval )
        ]

type alias ProjectWithJobsetsWithStatus =
    { project : Project
    , jobsets : List (JobsetWithStatus)
    }

decodeProjectWithJobsetsWithStatus : Decoder ProjectWithJobsetsWithStatus
decodeProjectWithJobsetsWithStatus =
    decode ProjectWithJobsetsWithStatus
        |> required "project" decodeProject
        |> required "jobsets" (list decodeJobsetWithStatus)

type alias JobsetWithStatus =
    { jobset : Jobset
    , jobsetStatus : JobsetStatus
    }

decodeJobsetWithStatus : Decoder JobsetWithStatus
decodeJobsetWithStatus =
    decode JobsetWithStatus
        |> required "jobset" decodeJobset
        |> required "jobsetStatus" decodeJobsetStatus

type alias JobsetStatus =
    { jobsetNrsucceeded : Int
    , jobsetNrfailed : Int
    , jobsetNrqueued : Int
    , jobsetLastevaluatedAt : Maybe (Int)
    }

decodeJobsetStatus : Decoder JobsetStatus
decodeJobsetStatus =
    decode JobsetStatus
        |> required "jobsetNrsucceeded" int
        |> required "jobsetNrfailed" int
        |> required "jobsetNrqueued" int
        |> required "jobsetLastevaluatedAt" (maybe int)

type alias Jobseteval =
    { jobsetevalId : Int
    , jobsetevalProject : String
    , jobsetevalJobset : String
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
        |> required "jobsetevalJobset" string
        |> required "jobsetevalTimestamp" int
        |> required "jobsetevalCheckouttime" int
        |> required "jobsetevalEvaltime" int
        |> required "jobsetevalHasnewbuilds" int
        |> required "jobsetevalHash" string
        |> required "jobsetevalNrbuilds" (maybe int)
        |> required "jobsetevalNrsucceeded" (maybe int)

type alias Job =
    { jobProject : String
    , jobJobset : String
    , jobName : String
    }

decodeJob : Decoder Job
decodeJob =
    decode Job
        |> required "jobProject" string
        |> required "jobJobset" string
        |> required "jobName" string

type alias JobsetevalWithStatus =
    { jobseteval : Jobseteval
    , jobsetevalChangedInputs : List (Jobsetevalinput)
    , jobsetevalSucceeded : Int
    , jobsetevalQueued : Int
    }

decodeJobsetevalWithStatus : Decoder JobsetevalWithStatus
decodeJobsetevalWithStatus =
    decode JobsetevalWithStatus
        |> required "jobseteval" decodeJobseteval
        |> required "jobsetevalChangedInputs" (list decodeJobsetevalinput)
        |> required "jobsetevalSucceeded" int
        |> required "jobsetevalQueued" int

type alias Jobsetevalinput =
    { jobsetevalinputEval : Int
    , jobsetevalinputName : String
    , jobsetevalinputAltnr : Int
    , jobsetevalinputType : String
    , jobsetevalinputUri : Maybe (String)
    , jobsetevalinputRevision : Maybe (String)
    , jobsetevalinputValue : Maybe (String)
    , jobsetevalinputDependency : Maybe (Int)
    , jobsetevalinputPath : Maybe (String)
    , jobsetevalinputSha256Hash : Maybe (String)
    }

decodeJobsetevalinput : Decoder Jobsetevalinput
decodeJobsetevalinput =
    decode Jobsetevalinput
        |> required "jobsetevalinputEval" int
        |> required "jobsetevalinputName" string
        |> required "jobsetevalinputAltnr" int
        |> required "jobsetevalinputType" string
        |> required "jobsetevalinputUri" (maybe string)
        |> required "jobsetevalinputRevision" (maybe string)
        |> required "jobsetevalinputValue" (maybe string)
        |> required "jobsetevalinputDependency" (maybe int)
        |> required "jobsetevalinputPath" (maybe string)
        |> required "jobsetevalinputSha256Hash" (maybe string)

type alias JobsetevalWithBuilds =
    { jobsetevalWithBuildsEval : Jobseteval
    , builds : List (Build)
    }

decodeJobsetevalWithBuilds : Decoder JobsetevalWithBuilds
decodeJobsetevalWithBuilds =
    decode JobsetevalWithBuilds
        |> required "jobsetevalWithBuildsEval" decodeJobseteval
        |> required "builds" (list decodeBuild)

type alias JobsetWithInputs =
    { jobsetWithInputsJobset : Jobset
    , jobsetWithInputsInputs : List (Jobsetinput)
    , jobsetWithInputsInputsalt : List (Jobsetinputalt)
    }

decodeJobsetWithInputs : Decoder JobsetWithInputs
decodeJobsetWithInputs =
    decode JobsetWithInputs
        |> required "jobsetWithInputsJobset" decodeJobset
        |> required "jobsetWithInputsInputs" (list decodeJobsetinput)
        |> required "jobsetWithInputsInputsalt" (list decodeJobsetinputalt)

encodeJobsetWithInputs : JobsetWithInputs -> Json.Encode.Value
encodeJobsetWithInputs x =
    Json.Encode.object
        [ ( "jobsetWithInputsJobset", encodeJobset x.jobsetWithInputsJobset )
        , ( "jobsetWithInputsInputs", (Json.Encode.list << List.map encodeJobsetinput) x.jobsetWithInputsInputs )
        , ( "jobsetWithInputsInputsalt", (Json.Encode.list << List.map encodeJobsetinputalt) x.jobsetWithInputsInputsalt )
        ]

type alias Jobsetinputalt =
    { jobsetinputaltProject : String
    , jobsetinputaltJobset : String
    , jobsetinputaltInput : String
    , jobsetinputaltAltnr : Int
    , jobsetinputaltValue : Maybe (String)
    , jobsetinputaltRevision : Maybe (String)
    }

decodeJobsetinputalt : Decoder Jobsetinputalt
decodeJobsetinputalt =
    decode Jobsetinputalt
        |> required "jobsetinputaltProject" string
        |> required "jobsetinputaltJobset" string
        |> required "jobsetinputaltInput" string
        |> required "jobsetinputaltAltnr" int
        |> required "jobsetinputaltValue" (maybe string)
        |> required "jobsetinputaltRevision" (maybe string)

encodeJobsetinputalt : Jobsetinputalt -> Json.Encode.Value
encodeJobsetinputalt x =
    Json.Encode.object
        [ ( "jobsetinputaltProject", Json.Encode.string x.jobsetinputaltProject )
        , ( "jobsetinputaltJobset", Json.Encode.string x.jobsetinputaltJobset )
        , ( "jobsetinputaltInput", Json.Encode.string x.jobsetinputaltInput )
        , ( "jobsetinputaltAltnr", Json.Encode.int x.jobsetinputaltAltnr )
        , ( "jobsetinputaltValue", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.string) x.jobsetinputaltValue )
        , ( "jobsetinputaltRevision", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.string) x.jobsetinputaltRevision )
        ]

type alias Jobsetinput =
    { jobsetinputProject : String
    , jobsetinputJobset : String
    , jobsetinputName : String
    , jobsetinputType : String
    , jobsetinputEmailresponsible : Int
    }

decodeJobsetinput : Decoder Jobsetinput
decodeJobsetinput =
    decode Jobsetinput
        |> required "jobsetinputProject" string
        |> required "jobsetinputJobset" string
        |> required "jobsetinputName" string
        |> required "jobsetinputType" string
        |> required "jobsetinputEmailresponsible" int

encodeJobsetinput : Jobsetinput -> Json.Encode.Value
encodeJobsetinput x =
    Json.Encode.object
        [ ( "jobsetinputProject", Json.Encode.string x.jobsetinputProject )
        , ( "jobsetinputJobset", Json.Encode.string x.jobsetinputJobset )
        , ( "jobsetinputName", Json.Encode.string x.jobsetinputName )
        , ( "jobsetinputType", Json.Encode.string x.jobsetinputType )
        , ( "jobsetinputEmailresponsible", Json.Encode.int x.jobsetinputEmailresponsible )
        ]

type alias Build =
    { buildId : Int
    , buildFinished : Int
    , buildTimestamp : Int
    , buildProject : String
    , buildJobset : String
    , buildJob : String
    , buildNixname : Maybe (String)
    , buildDescription : Maybe (String)
    , buildDrvpath : String
    , buildSystem : String
    , buildLicense : Maybe (String)
    , buildHomepage : Maybe (String)
    , buildMaintainers : Maybe (String)
    , buildMaxsilent : Maybe (Int)
    , buildTimeout : Maybe (Int)
    , buildIschannel : Int
    , buildIscurrent : Maybe (Int)
    , buildNixexprinput : Maybe (String)
    , buildNixexprpath : Maybe (String)
    , buildPriority : Int
    , buildGlobalpriority : Int
    , buildStarttime : Maybe (Int)
    , buildStoptime : Maybe (Int)
    , buildIscachedbuild : Maybe (Int)
    , buildBuildstatus : Maybe (Int)
    , buildSize : Maybe (Int)
    , buildClosuresize : Maybe (Int)
    , buildReleasename : Maybe (String)
    , buildKeep : Int
    }

decodeBuild : Decoder Build
decodeBuild =
    decode Build
        |> required "buildId" int
        |> required "buildFinished" int
        |> required "buildTimestamp" int
        |> required "buildProject" string
        |> required "buildJobset" string
        |> required "buildJob" string
        |> required "buildNixname" (maybe string)
        |> required "buildDescription" (maybe string)
        |> required "buildDrvpath" string
        |> required "buildSystem" string
        |> required "buildLicense" (maybe string)
        |> required "buildHomepage" (maybe string)
        |> required "buildMaintainers" (maybe string)
        |> required "buildMaxsilent" (maybe int)
        |> required "buildTimeout" (maybe int)
        |> required "buildIschannel" int
        |> required "buildIscurrent" (maybe int)
        |> required "buildNixexprinput" (maybe string)
        |> required "buildNixexprpath" (maybe string)
        |> required "buildPriority" int
        |> required "buildGlobalpriority" int
        |> required "buildStarttime" (maybe int)
        |> required "buildStoptime" (maybe int)
        |> required "buildIscachedbuild" (maybe int)
        |> required "buildBuildstatus" (maybe int)
        |> required "buildSize" (maybe int)
        |> required "buildClosuresize" (maybe int)
        |> required "buildReleasename" (maybe string)
        |> required "buildKeep" int

type alias QueueSummary =
    { queueSummaryJobsets : List (JobsetSummary)
    , queueSummarySystems : List (SystemSummary)
    , queueSummaryAll : Int
    , queueSummaryActif : Int
    }

decodeQueueSummary : Decoder QueueSummary
decodeQueueSummary =
    decode QueueSummary
        |> required "queueSummaryJobsets" (list decodeJobsetSummary)
        |> required "queueSummarySystems" (list decodeSystemSummary)
        |> required "queueSummaryAll" int
        |> required "queueSummaryActif" int

type alias JobsetSummary =
    { jobsetSummaryJobset : String
    , jobsetSummaryProject : String
    , jobsetSummaryQueued : Int
    }

decodeJobsetSummary : Decoder JobsetSummary
decodeJobsetSummary =
    decode JobsetSummary
        |> required "jobsetSummaryJobset" string
        |> required "jobsetSummaryProject" string
        |> required "jobsetSummaryQueued" int

type alias SystemSummary =
    { systemSummarySystem : String
    , systemSummaryQueued : Int
    }

decodeSystemSummary : Decoder SystemSummary
decodeSystemSummary =
    decode SystemSummary
        |> required "systemSummarySystem" string
        |> required "systemSummaryQueued" int

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
                , "projects"
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

getProjectsByProjectName : String -> String -> Http.Request (Maybe (Project))
getProjectsByProjectName urlBase capture_projectName =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ urlBase
                , "projects"
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

getProjectsWithJobsets : String -> Http.Request (List (ProjectWithJobsetsWithStatus))
getProjectsWithJobsets urlBase =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ urlBase
                , "projectsWithJobsets"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list decodeProjectWithJobsetsWithStatus)
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getProjectsWithJobsetsByProjectId : String -> String -> Http.Request (Maybe (ProjectWithJobsetsWithStatus))
getProjectsWithJobsetsByProjectId urlBase capture_projectId =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ urlBase
                , "projectsWithJobsets"
                , capture_projectId |> Http.encodeUri
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (maybe decodeProjectWithJobsetsWithStatus)
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getProjectsByProjectNameByJobsetNameJobsetevals : String -> String -> String -> Http.Request (List (JobsetevalWithStatus))
getProjectsByProjectNameByJobsetNameJobsetevals urlBase capture_projectName capture_jobsetName =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ urlBase
                , "projects"
                , capture_projectName |> Http.encodeUri
                , capture_jobsetName |> Http.encodeUri
                , "jobsetevals"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list decodeJobsetevalWithStatus)
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getProjectsByProjectNameByJobsetNameJobs : String -> String -> String -> Http.Request (List (JobsetevalWithBuilds))
getProjectsByProjectNameByJobsetNameJobs urlBase capture_projectName capture_jobsetName =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ urlBase
                , "projects"
                , capture_projectName |> Http.encodeUri
                , capture_jobsetName |> Http.encodeUri
                , "jobs"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list decodeJobsetevalWithBuilds)
        , timeout =
            Nothing
        , withCredentials =
            False
        }

postProjects : String -> Project -> Http.Request (String)
postProjects urlBase body =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ urlBase
                , "projects"
                ]
        , body =
            Http.jsonBody (encodeProject body)
        , expect =
            Http.expectJson string
        , timeout =
            Nothing
        , withCredentials =
            False
        }

postProjectsByProjectId : String -> String -> JobsetWithInputs -> Http.Request (String)
postProjectsByProjectId urlBase capture_projectId body =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ urlBase
                , "projects"
                , capture_projectId |> Http.encodeUri
                ]
        , body =
            Http.jsonBody (encodeJobsetWithInputs body)
        , expect =
            Http.expectJson string
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getQueue_summary : String -> Http.Request (QueueSummary)
getQueue_summary urlBase =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ urlBase
                , "queue_summary"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson decodeQueueSummary
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