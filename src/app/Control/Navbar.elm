module Control.Navbar exposing (Model, view)

import Html exposing (Html, a, button, div, nav, span, text)
import Html.Attributes exposing (attribute, class, title, type_)
import Html.Events exposing (onClick)
import Route exposing (Route, href)
import UUID


type alias Model msg =
    { currentPage : Route
    , currentProjectName : Maybe String
    , desktopSidebarVisible : Bool
    , onToggleDesktopSidebar : msg
    , onToggleMobileSidebar : msg
    }


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
                [ text "TODO Graph" ]
            , div [ class "d-flex align-items-center gap-2 ms-auto min-w-0" ]
                (projectTitle model)
            ]
        ]


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
