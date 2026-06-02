module Page.ProjectSelector exposing (Model, Msg(..), init, update, view)

import Browser exposing (Document)
import Html exposing (Html, a, button, li, text, ul)
import Html.Attributes exposing (disabled, href)
import Html.Events exposing (onClick)
import Platform.Cmd as Cmd
import Random
import Route exposing (Route)
import UUID exposing (UUID)


type alias Project =
    { id : UUID
    , name : Maybe String
    }


type State
    = ViewingProjects
    | GeneratingNew


type alias Model =
    { projects : List Project
    , state : State
    }


type Msg
    = OpenProject String
    | GenerateNewProject
    | NewProjectGenerated UUID


init : Model
init =
    { projects = []
    , state = ViewingProjects
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateNewProject ->
            let
                newModel =
                    { model | state = GeneratingNew }
            in
            let
                cmd =
                    Random.generate NewProjectGenerated UUID.generator
            in
            ( newModel, cmd )

        NewProjectGenerated id ->
            let
                newModel =
                    { model
                        | projects = { id = id, name = Nothing } :: model.projects
                        , state = ViewingProjects
                    }
            in
            ( newModel, Cmd.none )

        _ ->
            ( model, Cmd.none )


viewNewProjectButton : Model -> Html Msg
viewNewProjectButton model =
    case model.state of
        GeneratingNew ->
            button
                [ disabled True ]
                [ text "generating..." ]

        _ ->
            button
                [ onClick GenerateNewProject ]
                [ text "New project" ]


viewProjectListItem : Project -> Html Msg
viewProjectListItem project =
    let
        name =
            Maybe.withDefault
                ("Unnamed project " ++ UUID.toString project.id)
                project.name
    in
    li
        []
        [ a
            [ Route.href (Route.Project project.id) ]
            [ text name ]
        ]


view : Model -> Document Msg
view model =
    { title = "Projects"
    , body =
        [ text "Project list"
        , ul
            []
            (List.map
                viewProjectListItem
                model.projects
            )
        , viewNewProjectButton model
        ]
    }
