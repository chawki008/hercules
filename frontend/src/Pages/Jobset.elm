module Pages.Jobset exposing (..)

import Html.Events exposing (on, targetValue)
import Json.Decode  as Json
import Html exposing (..)
import Html.Attributes exposing (..)
import Material.List as List
import Material.Options as Options
import Material.Tabs as Tabs
import Material.Table as Table
import Material.Button as Button
import Material.Textfield as Textfield
import Material.Toggles as Toggles
import Models exposing (..)
import Msg exposing (..)
import Utils exposing (..)
import Urls as Urls exposing (..)
import String exposing(slice)
import Material.Icon as Icon
import Material.Grid exposing (grid, cell, size, Device(..))

view : AppModel -> Page -> List (Html Msg)
view model page =
    case page of 
        Jobset projectId jobsetName ->
            case model.jobsetPage of
                Err _ ->
                    [ p [] [ text "TODO" ] ]

                Ok jobset -> renderHeader model "Jobset" (Just jobset.name) Nothing
                        ++ [ Tabs.render Mdl
                                [ 4 ]
                                model.mdl
                                [ Tabs.ripple
                                , Tabs.onSelectTab SelectTab
                                , Tabs.activeTab jobset.selectedTab
                                ]
                                [ Tabs.label
                                    [ Options.center ]
                                    [ Options.span [ Options.css "width" "4px" ] []
                                    , text "Evaluations"
                                    ]
                                , Tabs.label
                                    [ Options.center ]
                                    [ Options.span [ Options.css "width" "4px" ] []
                                    , text "Jobs"
                                    ]
                                , Tabs.label
                                    [ Options.center ]
                                    [ Options.span [ Options.css "width" "4px" ] []
                                    , text "Channels"
                                    ]
                                ] (renderSelectedTab jobset)]
        newJobsetPage  -> 
            newJobsetView model                

renderSelectedTab : JobsetPage -> List (Html Msg)
renderSelectedTab  jobset = 
    case jobset.selectedTab of 
        1 -> jobsTab jobset
        2 -> [ p [] [ text "TODO" ] ]
        _ -> evaluationsTab  jobset
            
        
evaluationsTab :  JobsetPage -> List (Html Msg)
evaluationsTab  jobset = 
                         if List.isEmpty [ 1 ] then
                            render404 "No evaluations yet."
                         else
                            [ List.ul []
                                [ List.li [ List.withSubtitle ]
                                    [ List.content []
                                        [ text "Lastest check"
                                        , List.subtitle [] [ text jobset.latestCheckTime ]
                                        ]
                                    ]
                                , List.li [ List.withSubtitle ]
                                    [ List.content []
                                        [ text "Lastest evaluation"
                                        , List.subtitle []
                                            [ a
                                                [ href "TODO" ]
                                                [ text jobset.latestEvaluationTime ]
                                            ]
                                        ]
                                    ]
                                , List.li [ List.withSubtitle ]
                                    [ List.content []
                                        [ text "Lastest finished evaluation"
                                        , List.subtitle []
                                            [ a
                                                [ href "TODO" ]
                                                [ text jobset.latestFinishedEvaluationTime ]
                                            ]
                                        ]
                                    ]
                                ]
                            , Table.table [Options.css "width" "100%"]
                                [ Table.thead []
                                    [ Table.tr []
                                        [ Table.th [] [ text "#" ]
                                        , Table.th [] [ text "Input changes" ]
                                        , Table.th [] [ text "Job status" ]
                                        , Table.th [] [ text "Time" ]
                                        ]
                                    ]
                                , Table.tbody []
                                    (jobset.evaluations
                                        |> List.map
                                            (\evaluation ->
                                                Table.tr []
                                                    [ Table.td []
                                                        [ a
                                                            (onClickPage (Urls.Jobset "123" "foo"))
                                                            [ text (toString evaluation.id) ]
                                                        ]
                                                    , Table.td [Options.css  "white-space" "normal"] [ text evaluation.inputChanges ]
                                                    , Table.td [] (statusLabels evaluation.jobSummary.succeeded evaluation.jobSummary.failed evaluation.jobSummary.inQueue)
                                                    , Table.td [] [ text  evaluation.evaluatedAt ]
                                                    ]
                                            )
                                    )
                                ]
                            ]

jobsTab : JobsetPage -> List (Html Msg)
jobsTab jobset = 
                if List.isEmpty [ 1 ] then
                render404 "No Jobs yet."
                else
                [ 
                 Table.table [ Options.css "width" "100%"]
                    [ Table.thead []
                        [ Table.tr []
                            ([Table.th [] [ text "Job"]]
                              ++ ( ( List.take 11 jobset.evaluations)
                                        |> List.map 
                                            (\evaluation -> 
                                                Table.th [Options.css "padding" "0 0px 8px 0px"] [ text (slice 0 11 evaluation.evaluatedAt)]
                                            )))
                        ]
                    , Table.tbody []
                        (jobset.jobs 
                            |> List.map 
                                (\job -> 
                                    Table.tr []
                                         ([Table.td [] [text job.name]]
                                         ++ (job.infos
                                                |> List.map 
                                                    (\info ->
                                                        renderStatus info 
                                                    )))
                                
                                )
                        )
                    ]
                ]

renderStatus : (Int, Int, Int) -> (Html Msg)
renderStatus info = 
        case info of 
            (_,_,0) -> 
                Table.td [Options.css "color" "grey"] [  (Icon.view "help" [Icon.size24]) ]
            (_,0,_) -> 
                Table.td [Options.css "color" "green"] [  (Icon.view "checkboxmarkedcircle" [Icon.size18, Options.css "width" "1px"]) ]
            (_,_,_) -> 
                 Table.td [Options.css "color" "red"] [  (Icon.view "error" [Icon.size24]) ]
                                                                                                                                                        
newJobsetView : AppModel -> List (Html Msg)
newJobsetView model =
    let 
        project = Maybe.withDefault  (mapProjectWithJobsets Nothing) model.newProjectPage.project
    in
    renderHeader model "Add a new jobset" Nothing Nothing
        ++ [ Toggles.radio Mdl [0] model.mdl
                    [ Toggles.value (get 0 model.newJobsetPage.toggles)
                    , Toggles.group "EnabledRadioGroup"
                    , Toggles.ripple
                    , Toggles.onClick (SwitchToggle (NewJobset "") 0)
                    , Options.css "margin" "4px"
                    ]
                    [ text "Enabled" ]
                , Toggles.radio Mdl [1] model.mdl
                    [ Toggles.value (get 1 model.newJobsetPage.toggles)
                    , Toggles.group "EnabledRadioGroup"
                    , Toggles.ripple
                    , Toggles.onClick (SwitchToggle (NewJobset "")  1)
                    , Options.css "margin" "4px" 
                    ]
                    [ text "One-shot " ]
                , Toggles.radio Mdl [2] model.mdl
                    [ Toggles.value (get 2 model.newJobsetPage.toggles)
                    , Toggles.group "EnabledRadioGroup"
                    , Toggles.ripple
                    , Toggles.onClick (SwitchToggle (NewJobset "") 2)
                    , Options.css "margin" "4px"
                    ]
                    [ text "Disabled" ] 
                , Toggles.checkbox Mdl
                    [ 3 ]
                    model.mdl
                    [ Toggles.ripple
                    , Toggles.onClick (SwitchToggle (NewJobset "") 3)
                    , Toggles.value (get 3 model.newJobsetPage.toggles)
                    , Options.css "margin" "4px"
                    ]
                    [ text "Is visible" ]  
                , Html.form 
                    [ style [ ("margin", "4px")] ]
                    [ Textfield.render Mdl
                        [ 4 ]
                        model.mdl
                        [ Textfield.label "Identifier "
                        , Textfield.floatingLabel
                        , Textfield.text_
                        , Options.css "display" "block"
                        , Textfield.onInput (UpdateNewJobset "name")
                        ]
                    , Textfield.render Mdl
                        [ 5 ]
                        model.mdl
                        [ Textfield.label "Description "
                        , Textfield.floatingLabel
                        , Textfield.text_
                        , Textfield.onInput (UpdateNewJobset "description")
                        , Options.css "display" "block"
                        ]
                        , div 
                            []
                            [ Textfield.render Mdl
                                [ 6 ]
                                model.mdl
                                [ Textfield.label "Nix Expression"
                                , Textfield.floatingLabel
                                , Textfield.text_
                                , Textfield.onInput (UpdateNewJobset "nixexprpath")
                                , Options.css "margin-right" "20px"
                                , Options.css "float" "left"
                                ]
                            , Textfield.render Mdl
                                [ 7 ]
                                model.mdl
                                [ Textfield.label "In"
                                , Textfield.floatingLabel
                                , Textfield.text_
                                , Textfield.onInput (UpdateNewJobset "nixexprinput")
                                ]
                            ]
                    , Textfield.render Mdl
                        [ 8 ]
                        model.mdl
                        [ Textfield.label "Check Interval"
                        , Textfield.floatingLabel
                        , Textfield.text_
                        , Textfield.onInput (UpdateNewJobset "checkinterval")
                        , Options.css "display" "block"
                        ]
                    , Textfield.render Mdl
                        [ 9 ]
                        model.mdl
                        [ Textfield.label "Scheduling shares"
                        , Textfield.floatingLabel
                        , Textfield.text_
                        , Textfield.onInput (UpdateNewJobset "schedulingshares")
                        , Options.css "display" "block"
                        ]
                    ]
           , Toggles.checkbox Mdl
                [ 10 ]
                model.mdl
                [ Toggles.ripple
                , Toggles.onClick (SwitchToggle (NewJobset "") 4)
                , Toggles.value (get 4 model.newJobsetPage.toggles)
                ]
                [ text "Email notification" ]
           , Textfield.render Mdl
                [ 11 ]
                model.mdl
                [ Textfield.label "Email override"
                , Textfield.floatingLabel
                , Textfield.text_
                , Options.css "display" "block"
                , Textfield.onInput (UpdateNewJobset "emailoverride")
                ]
           , Textfield.render Mdl
                [ 12 ]
                model.mdl
                [ Textfield.label "Number of evaluations to Keep"
                , Textfield.floatingLabel
                , Textfield.text_
                , Options.css "display" "block"
                , Textfield.onInput (UpdateNewJobset "keepnr")
                ]
           , grid []
                  (
                    [cell [ Material.Grid.size All 4 ] [p [] [ text "Input name" ]]
                    , cell [ Material.Grid.size All 4 ] [p [] [ text "Type" ]]
                    , cell [ Material.Grid.size All 4 ] [p [] [ text "Values" ]]
                    ]
                    ++ List.concat 
                        ( List.range 0 model.newJobsetPage.jobsetInputsNr |> 
                            List.map 
                                (\index -> 
                                    [ cell [ Material.Grid.size All 4 ]
                                        [ Textfield.render Mdl
                                                            [ 15 + 2*index ]
                                                            model.mdl
                                                            [ Textfield.label "Input name"
                                                            , Textfield.floatingLabel
                                                            , Textfield.text_
                                                            , Options.css "display" "block"
                                                            , Textfield.onInput (UpdateNewJobsetInput "inputname" index)
                                                            ]
                                        ]
                                    , cell [ Material.Grid.size All 4 ]
                                        [ 
                                            div [ style [ ("position", "relative"), ("top", "0px")]]
                                                [ select [ on "change" (Json.map (UpdateNewJobsetInput "inputType" index) targetValue)
                                                , style [ ("margin", "10px")
                                                        , ("width", "150px")
                                                        , ("padding", "5px 35px 5px 5px")
                                                        , ("font-size", "16px")
                                                        , ("border", "1px solid #ccc")
                                                        , ("height", "34px")
                                                        , ("-webkit-appearance", "none")
                                                        , ("-moz-appearance", "none")
                                                        , ("appearance", "none")
                                                        ]
                                                ]
                                                    (List.map inputValOption ["Git Checkout", "Mercurial Checkout"])
                                                ]
                                        ]
                                    , cell [ Material.Grid.size All 4 ]
                                       (if index == model.newJobsetPage.jobsetInputsNr 
                                        then
                                            [ Button.render Mdl
                                                [ 14 ]
                                                model.mdl
                                                [ Button.fab
                                                , Button.colored
                                                , Button.onClick AddJobsetInput
                                                , Options.css "margin-left" "20px"
                                                , Options.css "float" "right"
                                                ]
                                                [ Icon.i "add" ]               
                                            , 
                                            Textfield.render Mdl
                                                                [ 16 + 2*index]
                                                                model.mdl
                                                                [ Textfield.label "value"
                                                                , Textfield.floatingLabel
                                                                , Textfield.text_
                                                                , Options.css "display" "block"
                                                                , Textfield.onInput (UpdateNewJobsetInput "value" index)
                                                                ]
                                            ]
                                        else
                                            [ Textfield.render Mdl
                                                        [ 16 + 2*index]
                                                        model.mdl
                                                        [ Textfield.label "value"
                                                        , Textfield.floatingLabel
                                                        , Textfield.text_
                                                        , Options.css "display" "block"
                                                        , Textfield.onInput (UpdateNewJobsetInput "value" index)
                                                        ]
                                            ]
                                        )
                                    ]
                                )
                            )
                  )
                  
           , Button.render Mdl
                [ 13 ]
                model.mdl
                [ Button.raised
                , Button.colored
                , Button.onClick AddJobsetSubmit
                ]
                [ text "Create Jobset" ]
           ]
 
inputValOption inputVal =
    option [ value inputVal ] [ text inputVal ]