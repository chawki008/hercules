module Urls exposing (..)

import Debug
import Navigation
import String
import UrlParser exposing (Parser, (</>), map, int, oneOf, s, string)


{-| Main type representing current url/page
-}
type Page
    = Home
    | Project String
    | NewProject
    | Jobset String String
    | NewJobset String 
    | QueueSummary

pageParser : Parser (Page -> a) a
pageParser =
    oneOf
        [ map Home (s "")
        , map Project (s "project" </> string)
        , map NewProject (s "create-project")
        , map Jobset (s "jobset" </> string </> string)
        , map NewJobset ( s"project" </> string </> s"create-jobset" )
        , map QueueSummary ( s"queue-summary")
        ]


pageToURL : Page -> String
pageToURL page =
    case page of
        Home ->
            "/"

        Project name ->
            "/project/" ++ name

        NewProject ->
            "/create-project"

        Jobset project jobset ->
            "/jobset/" ++ project ++ "/" ++ jobset

        NewJobset project -> 
            "/project/" ++ project ++ "/create-jobset" 

        QueueSummary -> 
            "/queue-summary"        
        
     


pageToTitle : Page -> String
pageToTitle page =
    case page of
        Home ->
            "Projects"

        Project name ->
            "Project " ++ name

        NewProject ->
            "New Project"
        
        NewJobset project ->
            "New Jobset " ++ project

        Jobset project jobset ->
            "Jobset " ++ jobset ++ " of project " ++ project

        QueueSummary -> 
            "Queue Summary"