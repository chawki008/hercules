module View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import List
import Material.Scheme
import Material.Color as Color
import Material.Layout as Layout
import Material.Options as Options
import Material.Footer as Footer
import Components.Navbar as Navbar
import Pages.Project exposing (..)
import Pages.Jobset as Jobset exposing (..)
import Pages.Queue as Queue exposing (..)
import Msg exposing (..)
import Models exposing (..)
import Utils exposing (..)
import Urls exposing (..)


view : AppModel -> Html Msg
view model =
    Options.div
        []
        [ Material.Scheme.topWithScheme Color.BlueGrey Color.LightBlue <|
            Layout.render Mdl
                model.mdl
                [ Layout.onSelectTab SelectLayoutTab
                , Layout.fixedHeader ]
                { header = Navbar.view model
                , drawer = []
                , tabs = ( Navbar.tabs model, [ Color.background (Color.color Color.LightBlue Color.A700) ] )
                , main = viewBody model
                }
        ]


viewBody : AppModel -> List (Html Msg)
viewBody model =
    let
        softwareLink name url version =
            p []
                [ a [ href url ]
                    [ text name ]
                , span []
                    [ text " @ "
                    , text version
                    ]
                ]
    in
        [Options.div
            [   Options.css "flex" "1"
                ,Options.css "position" "absolute"
                ,Options.css "height" "75%"
                ,Options.css "width" "100%"
            ]

            (
        Options.div
            [ Options.css "margin" "30px"
            , Options.css "min-height" "100%"
            ]
            (pageToView model)
            :: [ Footer.mini
                    [ Options.css "position" "relative"
                    , Options.css "bottom" "-70px"
                    , Options.css "width" "100%"
                    ]
                    { left =
                        Footer.left []
                            [ Footer.logo
                                []
                                []
                            ]
                    , right =
                        Footer.right []
                            [ Footer.logo
                                []
                                [ Footer.html <| softwareLink "Nix" "http://nixos.org/nix/" model.hydraConfig.nixVersion
                                , Footer.html <| softwareLink "Hydra" "http://nixos.org/hydra/" model.hydraConfig.hydraVersion
                                ]
                            ]
                    }
               ]
            )
        ]    


pageToView : AppModel -> List (Html Msg)
pageToView model =
    case model.currentPage of
        Home ->
            Pages.Project.view model model.currentPage

        Project name ->
            Pages.Project.view model model.currentPage

        NewProject ->
            Pages.Project.view model model.currentPage

        Jobset projectId jobsetName ->
                    case model.jobsetPage of
                        Ok _ ->
                            Jobset.view model model.currentPage

                        Err _ ->
                            render404 ("Jobset " ++ jobsetName ++ " does not exist.")

               
        NewJobset project -> 
                    Jobset.view model model.currentPage
        
        QueueSummary -> 
                    Queue.queueView model 