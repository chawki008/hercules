module Pages.Project exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Maybe
import List
import Material.Button as Button
import Material.Options as Options
import Material.Elevation as Elevation
import Material.Menu as Menu
import Material.Table as Table
import Material.Textfield as Textfield
import Material.Toggles as Toggles
import Components.LiveSearch exposing (search)
import Components.Help exposing (..)
import Msg exposing (..)
import Models exposing (Project, AppModel)
import Urls exposing (..)
import Utils exposing (..)


view : AppModel -> Page -> List (Html Msg)
view model page =
    case page of
        Home ->
            projectsView model model.projects

        Project id ->
            case List.head (List.filter (\p -> p.id == id) model.projects) of
                Just project ->
                    [ renderProjectWithDetails model 0 project ]

                Nothing ->
                    render404 ("Project does not exist.")

        NewProject ->
            newProjectView model

        -- TODO: get rid of this
        _ ->
            []


projectsView : AppModel -> List Project -> List (Html Msg)
projectsView model projects =
    let
        newprojects =
            List.indexedMap (renderProject model) (search projects)
    in
        (if   (model.user /= Nothing)
         then renderHeader model "Projects" Nothing (Just NewProject)
         else []
         )
            ++ if List.isEmpty newprojects then
                render404 "Zero projects. Maybe add one?"
               else
                newprojects


newProjectView : AppModel -> List (Html Msg)
newProjectView model =
    let 
        project = Maybe.withDefault  (mapProjectWithJobsets Nothing) model.newProjectPage.project
        projectIdError = get 0 model.newProjectPage.validations
    in
    renderHeader model "Add a new project" Nothing Nothing
        ++ [ Html.form []
                [ Textfield.render Mdl
                    [ 5 ]
                    model.mdl
                    [ Textfield.label "Identifier (e.g. hydra)"
                    , Textfield.floatingLabel
                    , Textfield.text_
                    , if projectIdError 
                      then Textfield.error "Invalid identifier: shouldn't start with number or special caracter and no spaces" 
                      else Options.css "display" "block"
                    , Textfield.value project.id
                    , Options.css "display" "block"
                    , Textfield.onInput (UpdateNewProject "id")
                    ]
                , Textfield.render Mdl
                    [ 6 ]
                    model.mdl
                    [ Textfield.label "Display name (e.g. Hydra)"
                    , Textfield.floatingLabel
                    , Textfield.text_
                    , Textfield.value project.name
                    , Textfield.onInput (UpdateNewProject "name")
                    , Options.css "display" "block"
                    ]
                , Textfield.render Mdl
                    [ 7 ]
                    model.mdl
                    [ Textfield.label "Description (e.g. Builds Nix expressions and provides insight about the process)"
                    , Textfield.floatingLabel
                    , Textfield.text_
                    , Textfield.value project.description
                    , Textfield.onInput (UpdateNewProject "description")
                    , Options.css "display" "block"
                    ]
                , Textfield.render Mdl
                    [ 8 ]
                    model.mdl
                    [ Textfield.label "URL (e.g. https://github.com/NixOS/hydra)"
                    , Textfield.floatingLabel
                    , Textfield.text_
                    , Textfield.value project.url
                    , Textfield.onInput (UpdateNewProject "url")
                    , Options.css "display" "block"
                    ]
                , Textfield.render Mdl
                    [ 13 ]
                    model.mdl
                    [ Textfield.label "repository"
                    , Textfield.floatingLabel
                    , Textfield.text_
                    , Textfield.value project.repo
                    , Textfield.onInput (UpdateNewProject "repo")
                    , Options.css "display" "block"
                    ]
                ]
           , Toggles.checkbox Mdl
                [ 9 ]
                model.mdl
                [ Toggles.ripple
                , Toggles.onClick (SwitchToggle NewProject 0)
                , Toggles.value (get 0 model.newProjectPage.toggles)
                ]
                [ text "Is visible on the project list?" ]
           , Toggles.checkbox Mdl
                [ 10 ]
                model.mdl
                [ Toggles.ripple
                , Toggles.onClick (SwitchToggle NewProject 1)
                , Toggles.value (get 1 model.newProjectPage.toggles)
                ]
                [ text "Is enabled?" ]
           , Textfield.render Mdl
                [ 11 ]
                model.mdl
                [ Textfield.label "Owner"
                , Textfield.floatingLabel
                , Textfield.text_
                , Options.css "display" "block"
                , Textfield.value (Maybe.withDefault "" (Maybe.map (\u -> u.name) model.user))
                , Textfield.onInput (UpdateNewProject "owner")
                ]
           , Button.render Mdl
                [ 12 ]
                model.mdl
                [ Button.raised
                , Button.colored
                , Button.onClick AddProjectSubmit
                ]
                [ text "Create project" ]
           ]



renderProjectWithDetails : AppModel -> Int -> Project -> Html Msg
renderProjectWithDetails model i project =
    Options.div
        [ Elevation.e2
        , Options.css "margin" "10px"
        , Options.css "padding" "4px"
        ]
        [ h5
            []
            [ a (onClickPage (Urls.Project project.id))
                [ Options.span
                    [ Options.css "margin" "16px" ]
                    [ text (project.name) ]
                ]
            , small
                [ class "hidden-xs" ]
                [ text ("(" ++ project.description ++ ")") ]
            , if ((model.user) /= Nothing) 
              then (Menu.render Mdl
                [i + 1]
                model.mdl
                [ Menu.ripple
                , Menu.bottomRight
                , Options.css "float" "right"
                ]
                [ Menu.item 
                    [ Menu.onSelect (NewPage (NewJobset (getProject i model).id )) ]
                    [ menuIcon "add"
                    , text "Add a jobset"
                    ]
                , Menu.item []
                    [ menuIcon "settings"
                    , text "Configuration"
                    ]
                , Menu.item []
                    [ menuIcon "delete"
                    , text "Delete the project"
                    ]
                ]) 
                else text ""
            ]
        , if List.isEmpty project.jobsets then
            Options.span
                [ Options.center
                , Options.css "margin" "30px"
                ]
                [ text "No Jobsets configured yet." ]
          else
            Table.table [ Options.css "width" "100%" ]
                [ Table.thead []
                    [ Table.tr []
                        [ Table.th [] [ text "Jobset", jobsetHelp model ]
                        , Table.th [] [ text "Description" ]
                        , Table.th [] [ text "Job status" ]
                        , Table.th [] [ text "Last evaluation" ]
                        ]
                    ]
                , Table.tbody []
                    (search project.jobsets
                        |> List.map
                            (\jobset ->
                                Table.tr []
                                    [ Table.td []
                                        [ a
                                            (onClickPage (Urls.Jobset project.id jobset.id))
                                            [ text jobset.name ]
                                        ]
                                    , Table.td [] [ text jobset.description ]
                                    , Table.td [] (statusLabels jobset.succeeded jobset.failed jobset.queued)
                                    , Table.td [] [ text jobset.lastEvaluation ]
                                    ]
                            )
                    )
                ]
        ]
renderProject : AppModel -> Int -> Project -> Html Msg
renderProject model i project =
    Options.div
        [ Elevation.e2
        , Options.css "margin" "10px"
        , Options.css "padding" "4px"
        ]
        [ h5
            []
            [ a (onClickPage (Urls.Project project.id))
                [ Options.span
                    [ Options.css "margin" "16px" ]
                    [ text (project.name) ]
                ]
            , small
                [ class "hidden-xs" ]
                [ text ("(" ++ project.description ++ ")") ]
              -- TODO: correct index
            , if ((model.user) /= Nothing) 
              then (Menu.render Mdl
                [i + 1]
                model.mdl
                [ Menu.ripple
                , Menu.bottomRight
                , Options.css "float" "right"
                ]
                [ Menu.item 
                    [ Menu.onSelect (NewPage (NewJobset (getProject i model).id )) ]
                    [ menuIcon "add"
                    , text "Add a jobset"
                    ]
                , Menu.item []
                    [ menuIcon "settings"
                    , text "Configuration"
                    ]
                , Menu.item []
                    [ menuIcon "delete"
                    , text "Delete the project"
                    ]
                ]) 
                else text ""
            ]
        ]

getProject : Int -> AppModel -> Project
getProject projectIndex model = Maybe.withDefault emptyProject (List.head (List.drop projectIndex model.projects))
