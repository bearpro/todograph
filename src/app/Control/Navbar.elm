module Control.Navbar exposing (Model, view)

import Html exposing (Html, a, div, li, nav, text, ul)
import Html.Attributes exposing (attribute, class)
import Route exposing (Route, href)
import UUID


type alias Model =
    { currentPage : Route
    , currentProjectName : Maybe String
    }


view : Model -> Html msg
view model =
    nav
        [ class "navbar navbar-expand-lg bg-body-tertiary mb-4" ]
        [ div
            [ class "container-fluid" ]
            [ a
                [ class "navbar-brand"
                , href Route.ProjectSelector
                ]
                [ text "TODO Graph" ]
            , div [ class "collapse navbar-collapse show" ]
                (ul [ class "navbar-nav" ]
                    [ navItem model Route.ProjectSelector "Projects" ]
                    :: projectTitle model
                )
            ]
        ]


projectTitle : Model -> List (Html msg)
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


navItem : Model -> Route -> String -> Html msg
navItem model route label =
    let
        isActive =
            model.currentPage == route

        activeAttributes =
            if isActive then
                [ class "nav-link active"
                , attribute "aria-current" "page"
                ]

            else
                [ class "nav-link" ]
    in
    li [ class "nav-item" ]
        [ a
            (href route :: activeAttributes)
            [ text label ]
        ]
