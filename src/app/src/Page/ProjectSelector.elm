module Page.ProjectSelector exposing (Model, Msg(..), ProjectSyncDisplay(..), init, projectSyncDisplay, update, view, viewPanel)

import Browser exposing (Document)
import Browser.Dom as Dom
import Control.FluentIcon as FluentIcon
import Domain.Project as DomainProject
import Html exposing (Attribute, Html, button, div, h2, input, li, text, ul)
import Html.Attributes as Attr exposing (class, disabled, title, type_, value)
import Html.Events exposing (on, onClick, onInput, stopPropagationOn)
import Json.Decode as Decode
import Platform.Cmd as Cmd
import Random
import Task
import UUID exposing (UUID)


type alias Project =
    { id : UUID
    , name : Maybe String
    , sync : Bool
    , syncPending : Bool
    }


type ProjectSyncDisplay
    = SyncOff
    | SyncClean
    | SyncPending


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
    | CloneProject UUID
    | ProjectCloned DomainProject.Project
    | StartProjectRename Project
    | ProjectNameDraftChanged String
    | SaveProjectName UUID
    | CancelProjectRename
    | ProjectNameInputFocused (Result Dom.Error ())
    | RequestProjectDelete UUID
    | ConfirmProjectDelete UUID
    | CancelProjectDelete
    | ToggleProjectSync UUID Bool
    | IgnoreClick


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
    , syncPending = project.syncPending
    }


projectSyncDisplay : DomainProject.Project -> ProjectSyncDisplay
projectSyncDisplay project =
    syncDisplay project.sync project.syncPending


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
                    { id = id, name = Nothing, sync = False, syncPending = False }

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

        CloneProject _ ->
            ( model, Cmd.none )

        ProjectCloned project ->
            let
                clonedProject =
                    projectSummary project

                newModel =
                    { model
                        | projects = clonedProject :: model.projects
                        , projectNameEdit =
                            Just
                                { id = clonedProject.id
                                , draft = Maybe.withDefault "" clonedProject.name
                                }
                        , projectDeleteConfirm = Nothing
                    }
            in
            ( newModel
            , Dom.focus (projectNameInputId clonedProject.id)
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
                    model.projectNameEdit
                        |> Maybe.andThen
                            (\edit ->
                                if edit.id == projectId then
                                    Nothing

                                else
                                    model.projectNameEdit
                            )
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

        IgnoreClick ->
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
        { project
            | sync = sync
            , syncPending =
                if sync then
                    project.syncPending

                else
                    False
        }

    else
        project


syncDisplay : Bool -> Bool -> ProjectSyncDisplay
syncDisplay sync syncPending =
    if not sync then
        SyncOff

    else if syncPending then
        SyncPending

    else
        SyncClean


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
        (projectListItemAttributes isActive isEditing isConfirmingDelete project)
        [ if isConfirmingDelete then
            viewProjectDeleteConfirm isActive project

          else if isEditing then
            viewProjectRename isActive project maybeEdit

          else
            viewProjectRow isActive project
        ]


projectListItemAttributes : Bool -> Bool -> Bool -> Project -> List (Attribute Msg)
projectListItemAttributes isActive isEditing isConfirmingDelete project =
    class
        ("list-group-item app-project-list-item"
            ++ (if isActive then
                    " app-project-list-item-active border-primary border-2"

                else
                    ""
               )
            ++ (if not isEditing && not isConfirmingDelete then
                    " app-project-list-item-clickable"

                else
                    ""
               )
        )
        :: (if not isEditing && not isConfirmingDelete then
                [ onClick (OpenProject (UUID.toString project.id))
                , Attr.attribute "role" "link"
                , Attr.attribute "tabindex" "0"
                , onEnter (OpenProject (UUID.toString project.id))
                ]

            else
                []
           )


viewProjectRow : Bool -> Project -> Html Msg
viewProjectRow isActive project =
    div [ class "min-w-0" ]
        [ div [ class "min-w-0" ]
            [ div
                [ class "d-block text-truncate text-body" ]
                [ text (projectDisplayName project) ]
            ]
        , viewProjectActionGroup project
            [ viewProjectSyncButton isActive project
            , viewProjectCloneButton isActive project
            , viewProjectRenameButton isActive project
            , viewProjectDeleteButton isActive project
            ]
        ]


viewProjectActionGroup : Project -> List (Html Msg) -> Html Msg
viewProjectActionGroup project actions =
    div
        [ class "btn-group btn-group-sm mt-2"
        , Attr.attribute "role" "group"
        , Attr.attribute "aria-label" ("Actions for " ++ projectDisplayName project)
        , stopClick IgnoreClick
        ]
        actions


stopClick : msg -> Attribute msg
stopClick msg =
    stopPropagationOn "click" (Decode.succeed ( msg, True ))


onClickStop : msg -> Attribute msg
onClickStop msg =
    stopClick msg


viewProjectSyncButton : Bool -> Project -> Html Msg
viewProjectSyncButton isActive project =
    let
        display =
            syncDisplay project.sync project.syncPending

        label =
            case display of
                SyncOff ->
                    "Enable sync for " ++ projectDisplayName project

                SyncClean ->
                    "Disable sync for " ++ projectDisplayName project

                SyncPending ->
                    "Waiting for sync. Disable sync for " ++ projectDisplayName project

        icon =
            case display of
                SyncOff ->
                    FluentIcon.CloudOff

                SyncClean ->
                    FluentIcon.CloudCheckmark

                SyncPending ->
                    FluentIcon.CloudSync
    in
    button
        [ onClickStop (ToggleProjectSync project.id (not project.sync))
        , type_ "button"
        , title label
        , Attr.attribute "aria-label" label
        , class (projectSyncButtonClass isActive display)
        ]
        [ FluentIcon.view icon ]


viewProjectCloneButton : Bool -> Project -> Html Msg
viewProjectCloneButton _ project =
    button
        [ onClickStop (CloneProject project.id)
        , type_ "button"
        , title ("Clone " ++ projectDisplayName project)
        , Attr.attribute "aria-label" ("Clone " ++ projectDisplayName project)
        , class "btn btn-outline-secondary btn-sm btn-icon"
        ]
        [ FluentIcon.view FluentIcon.Copy ]


projectSyncButtonClass : Bool -> ProjectSyncDisplay -> String
projectSyncButtonClass _ display =
    case display of
        SyncOff ->
            "btn btn-outline-secondary btn-sm btn-icon"

        SyncClean ->
            "btn btn-outline-success btn-sm btn-icon"

        SyncPending ->
            "btn btn-outline-warning btn-sm btn-icon"


viewProjectRenameButton : Bool -> Project -> Html Msg
viewProjectRenameButton _ project =
    button
        [ onClickStop (StartProjectRename project)
        , type_ "button"
        , title ("Rename " ++ projectDisplayName project)
        , Attr.attribute "aria-label" ("Rename " ++ projectDisplayName project)
        , class "btn btn-outline-secondary btn-sm btn-icon"
        ]
        [ FluentIcon.view FluentIcon.Edit ]


viewProjectDeleteButton : Bool -> Project -> Html Msg
viewProjectDeleteButton _ project =
    button
        [ onClickStop (RequestProjectDelete project.id)
        , type_ "button"
        , title ("Delete " ++ projectDisplayName project)
        , Attr.attribute "aria-label" ("Delete " ++ projectDisplayName project)
        , class "btn btn-outline-danger btn-sm btn-icon"
        ]
        [ FluentIcon.view FluentIcon.Delete ]


viewProjectDeleteConfirm : Bool -> Project -> Html Msg
viewProjectDeleteConfirm _ project =
    div [ class "min-w-0" ]
        [ div
            [ class "text-truncate" ]
            [ text (projectDisplayName project) ]
        , div
            [ class "btn-group btn-group-sm mt-2"
            , Attr.attribute "role" "group"
            , Attr.attribute "aria-label" ("Delete confirmation for " ++ projectDisplayName project)
            , stopClick IgnoreClick
            ]
            [ button
                [ onClickStop (ConfirmProjectDelete project.id)
                , type_ "button"
                , class "btn btn-danger btn-sm"
                ]
                [ text "Delete" ]
            , button
                [ onClickStop CancelProjectDelete
                , type_ "button"
                , class "btn btn-outline-secondary btn-sm"
                ]
                [ text "Cancel" ]
            ]
        ]


viewProjectRename : Bool -> Project -> Maybe ProjectNameEdit -> Html Msg
viewProjectRename _ project maybeEdit =
    let
        draft =
            maybeEdit
                |> Maybe.map .draft
                |> Maybe.withDefault ""
    in
    div
        [ class "min-w-0"
        , stopClick IgnoreClick
        ]
        [ input
            [ type_ "text"
            , Attr.id (projectNameInputId project.id)
            , value draft
            , onInput ProjectNameDraftChanged
            , onEnter (SaveProjectName project.id)
            , class "form-control form-control-sm"
            ]
            []
        , viewProjectActionGroup project
            [ button
                [ onClickStop (SaveProjectName project.id)
                , type_ "button"
                , class "btn btn-primary btn-sm"
                ]
                [ text "Save" ]
            , button
                [ onClickStop CancelProjectRename
                , type_ "button"
                , class "btn btn-outline-secondary btn-sm"
                ]
                [ text "Cancel" ]
            ]
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
