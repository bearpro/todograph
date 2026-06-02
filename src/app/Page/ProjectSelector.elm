module Page.ProjectSelector exposing (Model, Msg(..), init, update, view)

import Browser exposing (Document)
import Html exposing (Html, button, li, text, ul)
import Html.Attributes exposing (disabled)
import Html.Events exposing (onClick)
import Platform.Cmd as Cmd
import Random
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


view : Model -> Document Msg
view model =
    { title = "Projects"
    , body =
        [ text "Project list"
        , ul
            []
            (List.map
                (\p ->
                    let
                        name =
                            Maybe.withDefault
                                ("Unnamed project " ++ UUID.toString p.id)
                                p.name
                    in
                    li
                        []
                        [ text name ]
                )
                model.projects
            )
        , viewNewProjectButton model
        ]
    }
