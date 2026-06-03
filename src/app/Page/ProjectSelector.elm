module Page.ProjectSelector exposing (Model, Msg(..), init, update, view)

import Browser exposing (Document)
import Browser.Dom as Dom
import Html exposing (Attribute, Html, a, button, div, input, li, text, ul)
import Html.Attributes as Attr exposing (class, disabled, type_, value)
import Html.Events exposing (on, onClick, onInput)
import Json.Decode as Decode
import Platform.Cmd as Cmd
import Random
import Route
import Task
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
    , projectNameEdit : Maybe ProjectNameEdit
    }


type alias ProjectNameEdit =
    { id : UUID
    , draft : String
    }


type Msg
    = OpenProject String
    | GenerateNewProject
    | NewProjectGenerated UUID
    | StartProjectRename Project
    | ProjectNameDraftChanged String
    | SaveProjectName UUID
    | CancelProjectRename
    | ProjectNameInputFocused (Result Dom.Error ())


init : Model
init =
    { projects = []
    , state = ViewingProjects
    , projectNameEdit = Nothing
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OpenProject _ ->
            ( model, Cmd.none )

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

        StartProjectRename project ->
            ( { model
                | projectNameEdit =
                    Just
                        { id = project.id
                        , draft = Maybe.withDefault "" project.name
                        }
              }
            , Dom.focus (projectNameInputId project.id)
                |> Task.attempt ProjectNameInputFocused
            )

        ProjectNameDraftChanged draft ->
            let
                projectNameEdit =
                    model.projectNameEdit
                        |> Maybe.map (\edit -> { edit | draft = draft })
            in
            ( { model | projectNameEdit = projectNameEdit }, Cmd.none )

        SaveProjectName id ->
            let
                nextProjects =
                    model.projects
                        |> List.map (renameProject id model.projectNameEdit)
            in
            ( { model
                | projects = nextProjects
                , projectNameEdit = Nothing
              }
            , Cmd.none
            )

        CancelProjectRename ->
            ( { model | projectNameEdit = Nothing }, Cmd.none )

        ProjectNameInputFocused _ ->
            ( model, Cmd.none )


renameProject : UUID -> Maybe ProjectNameEdit -> Project -> Project
renameProject id maybeEdit project =
    case maybeEdit of
        Just edit ->
            if project.id == id && edit.id == id then
                { project | name = nameFromDraft edit.draft }

            else
                project

        Nothing ->
            project


nameFromDraft : String -> Maybe String
nameFromDraft draft =
    let
        trimmed =
            String.trim draft
    in
    if String.isEmpty trimmed then
        Nothing

    else
        Just trimmed


viewNewProjectButton : Model -> Html Msg
viewNewProjectButton model =
    case model.state of
        GeneratingNew ->
            button
                [ disabled True ]
                [ text "generating..." ]

        _ ->
            button
                [ onClick GenerateNewProject, class "btn btn-primary" ]
                [ text "New project" ]


projectDisplayName : Project -> String
projectDisplayName project =
    Maybe.withDefault
        ("Unnamed project " ++ UUID.toString project.id)
        project.name


viewProjectListItem : Maybe ProjectNameEdit -> Project -> Html Msg
viewProjectListItem maybeEdit project =
    let
        isEditing =
            maybeEdit
                |> Maybe.map (.id >> (==) project.id)
                |> Maybe.withDefault False
    in
    li
        [ class "list-group-item" ]
        [ if isEditing then
            viewProjectRename project maybeEdit

          else
            viewProjectRow project
        ]


viewProjectRow : Project -> Html Msg
viewProjectRow project =
    div [ class "d-flex align-items-center gap-2" ]
        [ a
            [ Route.href (Route.Project project.id), class "flex-grow-1" ]
            [ text (projectDisplayName project) ]
        , button
            [ onClick (StartProjectRename project)
            , class "btn btn-secondary btn-sm"
            ]
            [ text "Rename" ]
        ]


viewProjectRename : Project -> Maybe ProjectNameEdit -> Html Msg
viewProjectRename project maybeEdit =
    let
        draft =
            maybeEdit
                |> Maybe.map .draft
                |> Maybe.withDefault ""
    in
    div [ class "d-flex align-items-center gap-2" ]
        [ input
            [ type_ "text"
            , Attr.id (projectNameInputId project.id)
            , value draft
            , onInput ProjectNameDraftChanged
            , onEnter (SaveProjectName project.id)
            , class "form-control form-control-sm"
            ]
            []
        , button
            [ onClick (SaveProjectName project.id)
            , class "btn btn-primary btn-sm"
            ]
            [ text "Save" ]
        , button
            [ onClick CancelProjectRename
            , class "btn btn-outline-secondary btn-sm"
            ]
            [ text "Cancel" ]
        ]


projectNameInputId : UUID -> String
projectNameInputId projectId =
    "project-name-" ++ UUID.toString projectId


onEnter : msg -> Attribute msg
onEnter msg =
    on "keydown"
        (Decode.field "key" Decode.string
            |> Decode.andThen
                (\key ->
                    if key == "Enter" then
                        Decode.succeed msg

                    else
                        Decode.fail "Not Enter"
                )
        )


view : Model -> Document Msg
view model =
    { title = "Projects"
    , body =
        [ ul
            [ class "list-group" ]
            (List.map
                (viewProjectListItem model.projectNameEdit)
                model.projects
            )
        , viewNewProjectButton model
        ]
    }
