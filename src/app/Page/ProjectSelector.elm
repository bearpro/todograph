module Page.ProjectSelector exposing (Model, Msg(..), init, update, view, viewPanel)

import Browser exposing (Document)
import Browser.Dom as Dom
import Domain.Project as DomainProject
import Html exposing (Attribute, Html, a, button, div, h2, input, label, li, text, ul)
import Html.Attributes as Attr exposing (class, disabled, title, type_, value)
import Html.Events exposing (on, onCheck, onClick, onInput)
import Json.Decode as Decode
import Platform.Cmd as Cmd
import Random
import Route
import Task
import UUID exposing (UUID)


type alias Project =
    { id : UUID
    , name : Maybe String
    , sync : Bool
    }


type State
    = ViewingProjects
    | GeneratingNew


type alias Model =
    { projects : List Project
    , state : State
    , projectNameEdit : Maybe ProjectNameEdit
    , projectDeleteConfirm : Maybe UUID
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
    | RequestProjectDelete UUID
    | ConfirmProjectDelete UUID
    | CancelProjectDelete
    | ToggleProjectSync UUID Bool
    | CopyProjectLink UUID


init : List DomainProject.Project -> Model
init projects =
    { projects = List.map projectSummary projects
    , state = ViewingProjects
    , projectNameEdit = Nothing
    , projectDeleteConfirm = Nothing
    }


projectSummary : DomainProject.Project -> Project
projectSummary project =
    { id = project.id
    , name = project.name
    , sync = project.sync
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
                newProject =
                    { id = id, name = Nothing, sync = False }

                newModel =
                    { model
                        | projects = newProject :: model.projects
                        , state = ViewingProjects
                        , projectNameEdit =
                            Just
                                { id = id
                                , draft = ""
                                }
                        , projectDeleteConfirm = Nothing
                    }
            in
            ( newModel
            , Dom.focus (projectNameInputId id)
                |> Task.attempt ProjectNameInputFocused
            )

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

        RequestProjectDelete projectId ->
            ( { model | projectDeleteConfirm = Just projectId }
            , Cmd.none
            )

        ConfirmProjectDelete projectId ->
            ( { model
                | projects =
                    model.projects
                        |> List.filter (.id >> (/=) projectId)
                , projectDeleteConfirm = Nothing
                , projectNameEdit =
                    case model.projectNameEdit of
                        Just edit ->
                            if edit.id == projectId then
                                Nothing

                            else
                                model.projectNameEdit

                        Nothing ->
                            Nothing
              }
            , Cmd.none
            )

        CancelProjectDelete ->
            ( { model | projectDeleteConfirm = Nothing }
            , Cmd.none
            )

        ToggleProjectSync projectId sync ->
            ( { model | projects = List.map (setProjectSync projectId sync) model.projects }
            , Cmd.none
            )

        CopyProjectLink _ ->
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


setProjectSync : UUID -> Bool -> Project -> Project
setProjectSync id sync project =
    if project.id == id then
        { project | sync = sync }

    else
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
                [ disabled True
                , class "btn btn-primary"
                ]
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


viewProjectListItem : Maybe UUID -> Maybe ProjectNameEdit -> Maybe UUID -> Project -> Html Msg
viewProjectListItem activeProjectId maybeEdit deleteConfirm project =
    let
        isEditing =
            maybeEdit
                |> Maybe.map (.id >> (==) project.id)
                |> Maybe.withDefault False

        isActive =
            activeProjectId == Just project.id

        isConfirmingDelete =
            deleteConfirm == Just project.id
    in
    li
        [ class
            ("list-group-item"
                ++ (if isActive then
                        " active"

                    else
                        ""
                   )
            )
        ]
        [ if isConfirmingDelete then
            viewProjectDeleteConfirm isActive project

          else if isEditing then
            viewProjectRename isActive project maybeEdit

          else
            viewProjectRow isActive project
        ]


viewProjectRow : Bool -> Project -> Html Msg
viewProjectRow isActive project =
    div [ class "d-flex align-items-center gap-2" ]
        [ div [ class "flex-grow-1 min-w-0" ]
            [ a
                [ Route.href (Route.Project project.id)
                , onClick (OpenProject (UUID.toString project.id))
                , class
                    ("d-block text-truncate text-decoration-none"
                        ++ (if isActive then
                                " text-white"

                            else
                                " text-body"
                           )
                    )
                ]
                [ text (projectDisplayName project) ]
            , div [ class "d-flex align-items-center gap-2 mt-1" ]
                [ div [ class "form-check form-check-inline mb-0" ]
                    [ input
                        [ type_ "checkbox"
                        , Attr.id (syncCheckboxId project.id)
                        , Attr.checked project.sync
                        , onCheck (ToggleProjectSync project.id)
                        , class "form-check-input"
                        ]
                        []
                    , label
                        [ Attr.for (syncCheckboxId project.id)
                        , class
                            ("form-check-label small"
                                ++ (if isActive then
                                        " text-white"

                                    else
                                        " text-muted"
                                   )
                            )
                        ]
                        [ text "Sync" ]
                    ]
                , if project.sync then
                    button
                        [ onClick (CopyProjectLink project.id)
                        , class
                            (if isActive then
                                "btn btn-light btn-sm"

                             else
                                "btn btn-outline-secondary btn-sm"
                            )
                        , type_ "button"
                        ]
                        [ text "Copy link" ]

                  else
                    text ""
                ]
            ]
        , button
            [ onClick (StartProjectRename project)
            , class
                (if isActive then
                    "btn btn-light btn-sm"

                 else
                    "btn btn-secondary btn-sm"
                )
            ]
            [ text "Rename" ]
        , button
            [ onClick (RequestProjectDelete project.id)
            , type_ "button"
            , title ("Delete " ++ projectDisplayName project)
            , Attr.attribute "aria-label" ("Delete " ++ projectDisplayName project)
            , class
                (if isActive then
                    "btn btn-light btn-sm text-danger"

                 else
                    "btn btn-outline-danger btn-sm"
                )
            ]
            [ text "🗑️" ]
        ]


viewProjectDeleteConfirm : Bool -> Project -> Html Msg
viewProjectDeleteConfirm isActive project =
    div [ class "d-flex align-items-center gap-2" ]
        [ div
            [ class "flex-grow-1 text-truncate" ]
            [ text (projectDisplayName project) ]
        , button
            [ onClick (ConfirmProjectDelete project.id)
            , type_ "button"
            , class
                (if isActive then
                    "btn btn-light btn-sm text-danger"

                 else
                    "btn btn-danger btn-sm"
                )
            ]
            [ text "Delete" ]
        , button
            [ onClick CancelProjectDelete
            , type_ "button"
            , class
                (if isActive then
                    "btn btn-outline-light btn-sm"

                 else
                    "btn btn-outline-secondary btn-sm"
                )
            ]
            [ text "Cancel" ]
        ]


viewProjectRename : Bool -> Project -> Maybe ProjectNameEdit -> Html Msg
viewProjectRename isActive project maybeEdit =
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
            , class
                (if isActive then
                    "btn btn-outline-light btn-sm"

                 else
                    "btn btn-outline-secondary btn-sm"
                )
            ]
            [ text "Cancel" ]
        ]


projectNameInputId : UUID -> String
projectNameInputId projectId =
    "project-name-" ++ UUID.toString projectId


syncCheckboxId : UUID -> String
syncCheckboxId projectId =
    "project-sync-" ++ UUID.toString projectId


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
        [ viewPanel Nothing model ]
    }


viewPanel : Maybe UUID -> Model -> Html Msg
viewPanel activeProjectId model =
    div
        [ class "project-selector-panel d-flex flex-column gap-3" ]
        [ div
            [ class "d-flex align-items-center justify-content-between gap-3" ]
            [ h2
                [ class "h5 mb-0" ]
                [ text "Projects" ]
            , viewNewProjectButton model
            ]
        , ul
            [ class "list-group" ]
            (List.map
                (viewProjectListItem activeProjectId model.projectNameEdit model.projectDeleteConfirm)
                model.projects
            )
        ]
