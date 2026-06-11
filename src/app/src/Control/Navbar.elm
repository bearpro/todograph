module Control.Navbar exposing (Model, ServerStatus(..), view)

import Control.FluentIcon as FluentIcon
import Html exposing (Html, a, button, div, nav, span, text)
import Html.Attributes as Attr exposing (attribute, class, title, type_)
import Html.Events exposing (onClick)
import Route exposing (Route, href)
import UUID


type alias Model msg =
    { currentPage : Route
    , currentProjectName : Maybe String
    , serverStatus : ServerStatus
    , desktopSidebarVisible : Bool
    , onToggleDesktopSidebar : msg
    , onToggleMobileSidebar : msg
    }


type ServerStatus
    = Checking
    | ServerAvailable
    | ServerUnavailable


view : Model msg -> Html msg
view model =
    nav
        [ class "navbar navbar-expand-lg bg-body-tertiary sticky-top border-bottom" ]
        [ div
            [ class "container-fluid" ]
            [ button
                [ type_ "button"
                , class "navbar-toggler d-lg-none me-2"
                , title "Open projects"
                , attribute "aria-label" "Open projects"
                , onClick model.onToggleMobileSidebar
                ]
                [ span [ class "navbar-toggler-icon" ] [] ]
            , button
                [ type_ "button"
                , class "btn btn-outline-secondary btn-sm d-none d-lg-inline-flex me-2"
                , title
                    (if model.desktopSidebarVisible then
                        "Hide projects"

                     else
                        "Show projects"
                    )
                , attribute "aria-label"
                    (if model.desktopSidebarVisible then
                        "Hide projects"

                     else
                        "Show projects"
                    )
                , onClick model.onToggleDesktopSidebar
                ]
                [ span [ class "navbar-toggler-icon app-navbar-toggler-icon" ] [] ]
            , a
                [ class "navbar-brand"
                , href Route.ProjectSelector
                ]
                [ text "ToDo Graph" ]
            , githubLink
            , serverStatusIndicator model.serverStatus
            , div [ class "d-flex align-items-center gap-2 ms-auto min-w-0" ]
                (projectTitle model)
            ]
        ]


githubLink : Html msg
githubLink =
    a
        [ Attr.href "https://github.com/bearpro/todograph"
        , Attr.target "_blank"
        , Attr.rel "noopener noreferrer"
        , class "btn btn-outline-secondary btn-sm btn-icon"
        , title "GitHub repository"
        , attribute "aria-label" "GitHub repository"
        ]
        [ span [ class "app-navbar-icon app-navbar-github-icon" ] [] ]


serverStatusIndicator : ServerStatus -> Html msg
serverStatusIndicator status =
    let
        ( label, icon, statusClass ) =
            case status of
                Checking ->
                    ( "Checking server availability", FluentIcon.CloudSync, "btn-outline-secondary" )

                ServerAvailable ->
                    ( "Server is available", FluentIcon.CloudCheckmark, "btn-outline-success" )

                ServerUnavailable ->
                    ( "Server is unavailable", FluentIcon.CloudOff, "btn-outline-danger" )
    in
    span
        [ class ("btn btn-sm btn-icon app-navbar-status " ++ statusClass)
        , title label
        , attribute "aria-label" label
        , attribute "role" "img"
        ]
        [ FluentIcon.view icon ]


projectTitle : Model msg -> List (Html msg)
projectTitle model =
    case model.currentPage of
        Route.Project projectId ->
            let
                title =
                    model.currentProjectName
                        |> Maybe.map String.trim
                        |> Maybe.andThen
                            (\name ->
                                if String.isEmpty name then
                                    Nothing

                                else
                                    Just name
                            )
                        |> Maybe.withDefault (UUID.toString projectId)
            in
            [ div [ class "navbar-text ms-auto text-truncate" ]
                [ text title ]
            ]

        Route.ProjectSelector ->
            []
