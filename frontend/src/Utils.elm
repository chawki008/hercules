module Utils exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Material.Elevation as Elevation
import Material.Button as Button
import Material.Color as Color
import Material.Icon as Icon
import Material.Options as Options
import Msg exposing (..)
import Urls exposing (..)
import Models exposing (..) 
import Hercules as H
import Date
import String exposing (slice, dropRight)
import Array 
import Json.Encode
import Http


menuIcon : String -> Html Msg
menuIcon name =
    Icon.view name [ Options.css "width" "40px" ]


onPreventDefaultClick : msg -> Attribute msg
onPreventDefaultClick message =
    onWithOptions "click" { defaultOptions | preventDefault = True } (Json.succeed message)


onClickPage : Page -> List (Attribute Msg)
onClickPage page =
    [ style [ ( "pointer", "cursor" ) ]
    , href (pageToURL page)
    , onPreventDefaultClick (NewPage page)
    ]


optionalTag : Bool -> Html Msg -> Html Msg
optionalTag doInclude html =
    if doInclude then
        html
    else
        text ""


statusLabels : Int -> Int -> Int -> List (Html Msg)
statusLabels succeeded failed queued =
    [ optionalTag (succeeded > 0)
        (badge
            (Color.color Color.Green Color.S500)
            [ Options.attribute <| title "Jobs succeeded" ]
            [ text (toString succeeded) ]
        )
    , optionalTag (failed > 0)
        (badge
            (Color.color Color.Red Color.S500)
            [ Options.attribute <| title "Jobs failed" ]
            [ text (toString failed) ]
        )
    , optionalTag (queued > 0)
        (badge
            (Color.color Color.Grey Color.S500)
            [ Options.attribute <| title "Jobs in queue" ]
            [ text (toString queued) ]
        )
    ]


badge : Color.Color -> List (Options.Property c Msg) -> List (Html Msg) -> Html Msg
badge color properties content =
    Options.span
        ([ Options.css "border-radius" "9px"
         , Options.css "padding" "3px 5px"
         , Options.css "line-height" "14px"
         , Options.css "white-space" "nowrap"
         , Options.css "font-weight" "bold"
         , Options.css "font-size" "12px"
         , Options.css "margin" "0 3px"
         , Options.css "cursor" "help"
         , Options.css "color"
            (if color == Color.white then
                "#000"
             else
                "#FFF"
            )
         , Color.background color
         ]
            ++ properties
        )
        content


whiteBadge : List (Options.Property c Msg) -> List (Html Msg) -> Html Msg
whiteBadge properties content =
    badge Color.white properties content


render404 : String -> List (Html Msg)
render404 reason =
    [ Options.div
        [ Elevation.e2
        , Options.css "padding" "40px"
        , Options.center
        ]
        [ text reason ]
    ]


renderHeader : AppModel -> String -> Maybe String -> Maybe Page -> List (Html Msg)
renderHeader model name subname page =
    let
        subnameHtml =
            case subname of
                Nothing ->
                    []

                Just s ->
                    [ small [ style [ ( "margin-left", "10px" ) ] ]
                        [ text (String.toUpper s) ]
                    ]

        pageHtml =
            case page of
                Nothing ->
                    []

                Just p ->
                    [ Button.render Mdl
                        [ 2 ]
                        model.mdl
                        [ Button.fab
                        , Button.colored
                        , Button.onClick (NewPage p)
                        , Options.css "margin-left" "20px"
                        ]
                        [ Icon.i "add" ]
                    ]
    in
        [ h1
            [ style [ ( "margin-bottom", "30px" ), ("color", "rgb(30,50,900)"), ("font-style", "italic") ] ]
            ( subnameHtml ++ pageHtml)
        ]

mapProjectWithJobsets : Maybe H.ProjectWithJobsetsWithStatus -> Project 
mapProjectWithJobsets  maybeProjectWithJobsets = 

                    case maybeProjectWithJobsets of 

                        Just projectWithJobsets -> 
                            { id = projectWithJobsets.project.projectName
                            , name = projectWithJobsets.project.projectDisplayname
                            , description = Maybe.withDefault "" projectWithJobsets.project.projectDescription 
                            , isShown = not <| Maybe.withDefault False (boolFromInt projectWithJobsets.project.projectHidden)
                            , isEnabled = Maybe.withDefault False (boolFromInt projectWithJobsets.project.projectEnabled)
                            , jobsets = List.map mapJobset projectWithJobsets.jobsets
                            , owner = projectWithJobsets.project.projectOwner
                            , url = Maybe.withDefault "" projectWithJobsets.project.projectHomepage
                            }

                        Nothing ->  { id = ""
                                    , name = ""
                                    , description =""
                                    , isShown = False
                                    , isEnabled = False
                                    , jobsets = []
                                    , owner = ""
                                    , url = ""
                                    }
        
unMapProject : Project -> H.Project 
unMapProject project =  { projectName = project.id
                        , projectDisplayname = project.name
                        , projectDescription = Just project.description
                        , projectEnabled = if project.isEnabled then 1 else 0
                        , projectHidden = if project.isShown then 0 else 1
                        , projectOwner = project.owner
                        , projectHomepage = Just project.url
                        }

mapProject : H.Project -> Project
mapProject project = 
              { id = project.projectName
              , name = project.projectDisplayname
              , description = Maybe.withDefault "" project.projectDescription 
              , isShown = not <| Maybe.withDefault False (boolFromInt project.projectHidden)
              , isEnabled =  Maybe.withDefault False (boolFromInt project.projectEnabled)
              , jobsets = []
              , owner = project.projectOwner
              , url = Maybe.withDefault "" project.projectHomepage
              }

boolFromInt : Int -> Maybe Bool 
boolFromInt i = case i of 
                    0 -> Just False
                    1 -> Just True
                    _ -> Nothing

mapJobset : H.JobsetWithStatus -> Jobset
mapJobset jobsetWithStatus = 
    { id = jobsetWithStatus.jobset.jobsetName
    , name = jobsetWithStatus.jobset.jobsetName
    , description = Maybe.withDefault ""  jobsetWithStatus.jobset.jobsetDescription
    , queued =  jobsetWithStatus.jobsetStatus.jobsetNrqueued
    , failed =  jobsetWithStatus.jobsetStatus.jobsetNrfailed
    , succeeded =  jobsetWithStatus.jobsetStatus.jobsetNrsucceeded
    , isShown = True
    , lastEvaluation =  timestampToString (Maybe.withDefault 0 jobsetWithStatus.jobsetStatus.jobsetLastevaluatedAt)
    } 
                       
                      
mapJobsetevalsToJobsetPage : List (H.JobsetevalWithStatus) -> JobsetPage
mapJobsetevalsToJobsetPage jobsetevals = 
    let 
        latestJobsetEval = Maybe.withDefault  {jobseteval = { jobsetevalId = 0
                                                            , jobsetevalProject = ""
                                                            , jobsetevalJobset =  ""
                                                            , jobsetevalTimestamp = 0
                                                            , jobsetevalCheckouttime = 0
                                                            , jobsetevalEvaltime = 0
                                                            , jobsetevalHasnewbuilds = 0
                                                            , jobsetevalHash = ""
                                                            , jobsetevalNrbuilds = Just 0
                                                            , jobsetevalNrsucceeded = Just 0
                                                            } 
                                              , jobsetevalChangedInputs = []              
                                              , jobsetevalSucceeded = 0
                                              , jobsetevalQueued = 0  
                                              }
                                            (List.head jobsetevals) 
    in
        { latestCheckTime = "2018-Feb-14 21:38:01"
        , latestEvaluationTime = timestampToString latestJobsetEval.jobseteval.jobsetevalTimestamp
        , latestFinishedEvaluationTime = "2018-Feb-14 12:38:01"
        , evaluations = List.map mapJobsetevalToEval jobsetevals
        , name = latestJobsetEval.jobseteval.jobsetevalJobset
        , selectedTab = 0
        , project = latestJobsetEval.jobseteval.jobsetevalProject
        , jobs = []
        }

mapJobsetevalToEval : H.JobsetevalWithStatus -> Evaluation
mapJobsetevalToEval jobsetevalwithstatus =  
    { id = jobsetevalwithstatus.jobseteval.jobsetevalId
    , inputChanges = strFromEvalInputs jobsetevalwithstatus.jobsetevalChangedInputs
    , jobSummary = { succeeded = jobsetevalwithstatus.jobsetevalSucceeded
                , failed = (Maybe.withDefault 0 jobsetevalwithstatus.jobseteval.jobsetevalNrbuilds) - jobsetevalwithstatus.jobsetevalSucceeded - jobsetevalwithstatus.jobsetevalQueued
                , inQueue  = jobsetevalwithstatus.jobsetevalQueued 
                }
    , evaluatedAt = timestampToString jobsetevalwithstatus.jobseteval.jobsetevalTimestamp
    }  
strFromEvalInputs : List (H.Jobsetevalinput) -> String
strFromEvalInputs changedInputs = dropRight 2 (List.foldl printInput "" changedInputs)

printInput : H.Jobsetevalinput -> String -> String 
printInput input result = result ++ input.jobsetevalinputName ++ " → " ++ (renderShortInput input)  ++ ", "

renderShortInput : H.Jobsetevalinput -> String 
renderShortInput input = let 
                            revision = Maybe.withDefault "" input.jobsetevalinputRevision   
                        in
                            case input.jobsetevalinputType of      
                                    "svn" -> revision
                                    "svn-checkout" -> revision
                                    "bzr" -> revision
                                    "bzr-checkout" -> revision
                                    "git" -> slice 0 7 revision
                                    "hg" -> slice 0 12 revision
                                    -- "build" -> slice 0 12 revision
                                    _ -> revision
                                
timestampToString : Int -> String         
timestampToString timstamp = 
    let 
        evaluatedAt = Date.fromTime (toFloat (timstamp*1000))
        year = toString (Date.year evaluatedAt)
        month = toString (Date.month evaluatedAt)
        day = toString (Date.day evaluatedAt)
        hours = toString (Date.hour evaluatedAt)
        minutes = toString (Date.minute evaluatedAt)
        seconds  = toString (Date.second  evaluatedAt)
    in 
        year ++ "-" ++ month ++ "-" ++ day ++ " " ++ hours ++ ":" ++ minutes ++ ":" ++ seconds

mapToJobs :  List (H.JobsetevalWithBuilds) -> List (Job)
mapToJobs jobsetevalWithBuilds = List.map (mapToJob jobsetevalWithBuilds) (getAllJobsNames jobsetevalWithBuilds)

mapToJob : List (H.JobsetevalWithBuilds) -> String -> Job
mapToJob jobsetevalWithBuilds job = 
                                { name = job
                                , infos = List.reverse <| getJobsInfo jobsetevalWithBuilds job
                                }
getAllJobsNames : List (H.JobsetevalWithBuilds) -> List (String)
getAllJobsNames jobsetevalsWithBuilds = getJobNames (List.head jobsetevalsWithBuilds)

getJobNames : Maybe (H.JobsetevalWithBuilds) -> List (String) 
getJobNames jobsetevalWithBuilds = case jobsetevalWithBuilds of 
                                            Nothing -> []
                                            Just jbewbs -> List.map getJobName jbewbs.builds

getJobName : H.Build -> String 
getJobName build = build.buildJob

getJobsInfo :   List (H.JobsetevalWithBuilds) -> String -> List (Int, Int, Int)
getJobsInfo jobsetevalsWithBuilds job = List.map (getJobInfoFromEval job) jobsetevalsWithBuilds 

getJobInfoFromEval : String -> H.JobsetevalWithBuilds -> (Int, Int, Int)
getJobInfoFromEval job jobsetevalsWithBuilds = let
                                                    emptyBuild = 
                                                            { buildId = 0
                                                            , buildFinished = 0
                                                            , buildTimestamp = 0
                                                            , buildProject = ""
                                                            , buildJobset = ""
                                                            , buildJob = ""
                                                            , buildNixname = Nothing
                                                            , buildDescription = Nothing
                                                            , buildDrvpath = ""
                                                            , buildSystem = ""
                                                            , buildLicense = Nothing
                                                            , buildHomepage = Nothing
                                                            , buildMaintainers = Nothing
                                                            , buildMaxsilent = Nothing
                                                            , buildTimeout = Nothing
                                                            , buildIschannel = 0
                                                            , buildIscurrent = Nothing
                                                            , buildNixexprinput = Nothing
                                                            , buildNixexprpath = Nothing
                                                            , buildPriority = 0
                                                            , buildGlobalpriority = 0
                                                            , buildStarttime = Nothing
                                                            , buildStoptime = Nothing
                                                            , buildIscachedbuild = Nothing
                                                            , buildBuildstatus = Nothing
                                                            , buildSize = Nothing
                                                            , buildClosuresize = Nothing
                                                            , buildReleasename = Nothing
                                                            , buildKeep = 0
                                                            }
                                                    jobBuild = Maybe.withDefault emptyBuild (List.head (List.filter (\build -> build.buildJob == job) jobsetevalsWithBuilds.builds))
                                                    jobStatus = Maybe.withDefault 1 jobBuild.buildBuildstatus
                                                    jobFinished = jobBuild.buildFinished
                                               in
                                                    (jobsetevalsWithBuilds.jobsetevalWithBuildsEval.jobsetevalTimestamp, jobStatus, jobFinished)

emptyProject : Project 
emptyProject =  { id = ""
                , name = ""
                , description = ""
                , isShown =  False
                , isEnabled =  False
                , jobsets = []
                , owner = ""
                , url = ""
                , repo = ""
                }
                
emptyJobsetWithInputs : JobsetWithInputs 
emptyJobsetWithInputs = { name = ""
                        , enabled = 0
                        , hidden = False
                        , description = ""
                        , nixexprinput = ""
                        , nixexprpath = ""
                        , checkinterval = 1
                        , schedulingshares = 1
                        , enableemail = False
                        , emailoverride = ""
                        , keepnr = 1
                        , inputs  = Array.fromList [emptyJobsetInput]
                        }

emptyJobsetInput : JobsetInput
emptyJobsetInput =  { inputname = ""
                    , inputType = "Git"
                    , value = ""
                    , emailResponsible = False
                    }              

get : Int -> Array.Array Bool -> Bool
get k toggles = 
  Array.get k toggles |> Maybe.withDefault False

getJobsetInput : Int -> Array.Array JobsetInput -> JobsetInput
getJobsetInput k jobsetInputs = 
  Array.get k jobsetInputs |> Maybe.withDefault emptyJobsetInput


encodeJobsetWithInputs : JobsetWithInputs -> Json.Encode.Value
encodeJobsetWithInputs x =
    Json.Encode.object
        [ ( "name", Json.Encode.string x.name)
        , ( "enabled", Json.Encode.int x.enabled)
        , ( "hidden", Json.Encode.bool x.hidden)
        , ( "description", Json.Encode.string x.description)
        , ( "nixexprinput", Json.Encode.string x.nixexprinput)
        , ( "nixexprpath", Json.Encode.string x.nixexprpath)
        , ( "checkinterval", Json.Encode.int x.checkinterval)
        , ( "schedulingshares", Json.Encode.int x.schedulingshares)
        , ( "enableemail", Json.Encode.bool x.enableemail)
        , ( "emailoverride", Json.Encode.string x.emailoverride)
        , ( "keepnr", Json.Encode.int x.keepnr)
        , ( "inputs",  jsonInputs x.inputs) 
        ]
        
jsonInputs : Array.Array JobsetInput -> Json.Encode.Value
jsonInputs = Json.Encode.object << Array.toList << Array.map jsonInput 

jsonInput : JobsetInput -> (String, Json.Encode.Value) 
jsonInput jobsetInput = (jobsetInput.inputname, Json.Encode.object [("type", Json.Encode.string jobsetInput.inputType), ("value", Json.Encode.string jobsetInput.value), ("emailresponsible", Json.Encode.bool jobsetInput.emailResponsible)])

postJobset : String -> String -> JobsetWithInputs -> Http.Request (String)
postJobset urlBase capture_projectId body =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ urlBase
                , "projects"
                , (Debug.log "ss" capture_projectId) |> Http.encodeUri
                ]
        , body =
            Http.jsonBody (encodeJobsetWithInputs body)
        , expect =
            Http.expectJson Json.string
        , timeout =
            Nothing
        , withCredentials =
            False
        }