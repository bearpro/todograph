module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Control.Navbar
import Domain.Project as Project
import Html exposing (Html)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Json.Encode as Encode
import Page.AppInit as AppInitPage
import Page.ProjectSelector as ProjectSelectorPage
import Page.TodoGraph as TodoGraphPage
import Ports.ProjectStorage as ProjectStorage
import Ports.ProjectSync as ProjectSync
import Random
import Route exposing (Route)
import Task
import Time
import UUID exposing (UUID)
import Url exposing (Url)


type Page
    = AppInit AppInitPage.Model
    | TodoGraph TodoGraphPage.Model
    | ProjectLoading UUID
    | ProjectLoadFailed UUID String
    | NoProjectSelected


type alias Model =
    { page : Page
    , key : Nav.Key
    , projects : List Project.Project
    , projectSelector : ProjectSelectorPage.Model
    , route : Maybe Route
    , storageError : Maybe String
    , desktopSidebarVisible : Bool
    , mobileSidebarOpen : Bool
    }


type Msg
    = ClickedLink Browser.UrlRequest
    | ChangedUrl Url
    | ProjectSelectorMsg ProjectSelectorPage.Msg
    | TodoGraphMsg TodoGraphPage.Msg
    | ProjectsLoaded Decode.Value
    | StorageFailed String
    | FirstProjectGenerated UUID UUID
    | ProjectCloned UUID UUID
    | ProjectTimestampedForSave UUID Time.Posix
    | ServerProjectsChecked Decode.Value
    | ServerProjectVersionLoaded Decode.Value
    | ServerProjectLoaded Decode.Value
    | ServerProjectSaveAccepted Decode.Value
    | ServerProjectSaveRejected Decode.Value
    | ServerProjectUpdated Decode.Value
    | SyncFailed Decode.Value
    | ToggleDesktopSidebar
    | ToggleMobileSidebar
    | CloseMobileSidebar


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init () url navKey =
    ( { page = AppInit AppInitPage.init
      , key = navKey
      , projects = []
      , projectSelector = ProjectSelectorPage.init []
      , route = Route.fromUrl url
      , storageError = Nothing
      , desktopSidebarVisible = True
      , mobileSidebarOpen = False
      }
    , ProjectStorage.loadProjects ()
    )


currentRoute : Page -> Route
currentRoute page =
    case page of
        AppInit _ ->
            Route.ProjectSelector

        NoProjectSelected ->
            Route.ProjectSelector

        TodoGraph model ->
            Route.Project model.project.id

        ProjectLoading projectId ->
            Route.Project projectId

        ProjectLoadFailed projectId _ ->
            Route.Project projectId


currentProjectName : Page -> Maybe String
currentProjectName page =
    case page of
        TodoGraph model ->
            model.project.name

        _ ->
            Nothing


viewWorkspace : Page -> Browser.Document Msg
viewWorkspace page =
    case page of
        AppInit model ->
            mapDocument identity (AppInitPage.view model)

        TodoGraph model ->
            mapDocument TodoGraphMsg (TodoGraphPage.view model)

        ProjectLoading _ ->
            { title = "Loading project"
            , body =
                [ Html.div
                    [ class "p-4" ]
                    [ Html.text "Loading project..." ]
                ]
            }

        ProjectLoadFailed _ message ->
            { title = "Project not found"
            , body =
                [ Html.div
                    [ class "p-4 text-danger" ]
                    [ Html.text message ]
                ]
            }

        NoProjectSelected ->
            { title = "Projects"
            , body = []
            }


mapDocument : (childMsg -> parentMsg) -> Browser.Document childMsg -> Browser.Document parentMsg
mapDocument toParent document =
    { title = document.title
    , body = List.map (Html.map toParent) document.body
    }


viewProjectSelectorPanel : Model -> Html Msg
viewProjectSelectorPanel model =
    ProjectSelectorPage.viewPanel (activeProjectId model.page) model.projectSelector
        |> Html.map ProjectSelectorMsg


activeProjectId : Page -> Maybe UUID
activeProjectId page =
    case page of
        TodoGraph todoGraphModel ->
            Just todoGraphModel.project.id

        _ ->
            Nothing


viewDesktopSidebar : Model -> Html Msg
viewDesktopSidebar model =
    Html.aside
        [ class
            ("app-sidebar"
                ++ (if model.desktopSidebarVisible then
                        ""

                    else
                        " app-sidebar-hidden"
                   )
            )
        ]
        [ viewProjectSelectorPanel model ]


viewMobileSidebar : Model -> List (Html Msg)
viewMobileSidebar model =
    if model.mobileSidebarOpen then
        [ Html.div
            [ class "app-sidebar-backdrop d-lg-none"
            , onClick CloseMobileSidebar
            ]
            []
        , Html.aside
            [ class "app-mobile-sidebar d-lg-none" ]
            [ Html.div
                [ class "d-flex justify-content-end px-3 pt-3" ]
                [ Html.button
                    [ class "btn-close"
                    , onClick CloseMobileSidebar
                    ]
                    []
                ]
            , viewProjectSelectorPanel model
            ]
        ]

    else
        []


viewShell : Model -> Browser.Document Msg -> Browser.Document Msg
viewShell model workspace =
    { title = "ToDo Graph | " ++ workspace.title
    , body =
        [ Control.Navbar.view
            { currentPage = currentRoute model.page
            , currentProjectName = currentProjectName model.page
            , desktopSidebarVisible = model.desktopSidebarVisible
            , onToggleDesktopSidebar = ToggleDesktopSidebar
            , onToggleMobileSidebar = ToggleMobileSidebar
            }
        , Html.div
            [ class "app-layout" ]
            ([ viewDesktopSidebar model
             , Html.main_
                [ class "app-main" ]
                workspace.body
             ]
                ++ viewMobileSidebar model
            )
        ]
    }


view : Model -> Browser.Document Msg
view model =
    viewShell model (viewWorkspace model.page)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ ProjectStorage.projectsLoaded ProjectsLoaded
        , ProjectStorage.storageFailed StorageFailed
        , ProjectSync.serverProjectsChecked ServerProjectsChecked
        , ProjectSync.serverProjectVersionLoaded ServerProjectVersionLoaded
        , ProjectSync.serverProjectLoaded ServerProjectLoaded
        , ProjectSync.serverProjectSaveAccepted ServerProjectSaveAccepted
        , ProjectSync.serverProjectSaveRejected ServerProjectSaveRejected
        , ProjectSync.serverProjectUpdated ServerProjectUpdated
        , ProjectSync.syncFailed SyncFailed
        , case model.page of
            TodoGraph page ->
                TodoGraphPage.subscriptions page
                    |> Sub.map TodoGraphMsg

            _ ->
                Sub.none
        ]


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    case maybeRoute of
        Nothing ->
            ( { model
                | page = NoProjectSelected
                , projectSelector = ProjectSelectorPage.init model.projects
                , route = Just Route.ProjectSelector
                , mobileSidebarOpen = False
              }
            , Cmd.batch
                [ unsubscribeCurrentProjectExcept Nothing model
                , Nav.replaceUrl model.key "/"
                ]
            )

        Just Route.ProjectSelector ->
            ( { model
                | page = NoProjectSelected
                , projectSelector = ProjectSelectorPage.init model.projects
                , route = maybeRoute
                , mobileSidebarOpen = False
              }
            , unsubscribeCurrentProjectExcept Nothing model
            )

        Just (Route.Project projectId) ->
            case findProject projectId model.projects of
                Just project ->
                    let
                        ( todoGraphModel, cmd ) =
                            initTodoGraph project
                    in
                    ( { model
                        | page = TodoGraph todoGraphModel
                        , route = maybeRoute
                        , mobileSidebarOpen = False
                      }
                    , Cmd.batch
                        [ unsubscribeCurrentProjectExcept (Just project.id) model
                        , Cmd.map TodoGraphMsg cmd
                        , projectOpenSyncCmd project
                        ]
                    )

                Nothing ->
                    ( { model
                        | page = ProjectLoading projectId
                        , projectSelector = ProjectSelectorPage.init model.projects
                        , route = maybeRoute
                        , mobileSidebarOpen = False
                      }
                    , Cmd.batch
                        [ unsubscribeCurrentProjectExcept Nothing model
                        , fetchServerProjectCmd projectId
                        ]
                    )


initTodoGraph : Project.Project -> ( TodoGraphPage.Model, Cmd TodoGraphPage.Msg )
initTodoGraph project =
    TodoGraphPage.init
        { project = project
        , nodeUiStates = []
        , joinDrag = Nothing
        , now = Nothing
        , openAddMenu = Nothing
        , nodeHeights = []
        }


findProject : UUID -> List Project.Project -> Maybe Project.Project
findProject projectId projects =
    projects
        |> List.filter (.id >> (==) projectId)
        |> List.head


upsertProject : Project.Project -> List Project.Project -> List Project.Project
upsertProject nextProject projects =
    let
        replace project =
            if project.id == nextProject.id then
                nextProject

            else
                project

        exists =
            projects
                |> List.any (.id >> (==) nextProject.id)
    in
    if exists then
        projects |> List.map replace

    else
        nextProject :: projects


saveProjectCmd : Project.Project -> Cmd Msg
saveProjectCmd project =
    ProjectStorage.saveProject (Project.projectEncoder project)


deleteProjectCmd : UUID -> Cmd Msg
deleteProjectCmd projectId =
    ProjectStorage.deleteProject (UUID.toString projectId)


projectIdString : UUID -> String
projectIdString projectId =
    UUID.toString projectId


projectUpdatedAtMillis : Project.Project -> Int
projectUpdatedAtMillis project =
    Time.posixToMillis project.updatedAt


serverProjectEnvelopeEncoder : Project.Project -> Encode.Value
serverProjectEnvelopeEncoder project =
    Encode.object
        [ ( "projectId", Encode.string (projectIdString project.id) )
        , ( "updatedAt", Encode.int (projectUpdatedAtMillis project) )
        , ( "payload", Project.projectPayloadEncoder project )
        ]


serverProjectCheckEncoder : Project.Project -> Encode.Value
serverProjectCheckEncoder project =
    Encode.object
        [ ( "projectId", Encode.string (projectIdString project.id) )
        , ( "updatedAt", Encode.int (projectUpdatedAtMillis project) )
        ]


saveServerProjectCmd : Project.Project -> Cmd Msg
saveServerProjectCmd project =
    ProjectSync.saveServerProject (serverProjectEnvelopeEncoder project)


debounceSaveServerProjectCmd : Project.Project -> Cmd Msg
debounceSaveServerProjectCmd project =
    ProjectSync.debounceSaveServerProject (serverProjectEnvelopeEncoder project)


debounceSaveSyncedProjectCmd : Project.Project -> Cmd Msg
debounceSaveSyncedProjectCmd project =
    if project.sync then
        debounceSaveServerProjectCmd project

    else
        Cmd.none


checkSyncedProjectsCmd : List Project.Project -> Cmd Msg
checkSyncedProjectsCmd projects =
    let
        syncedProjects =
            projects
                |> List.filter .sync
    in
    if List.isEmpty syncedProjects then
        Cmd.none

    else
        ProjectSync.checkServerProjects
            (Encode.list serverProjectCheckEncoder syncedProjects)


fetchServerProjectCmd : UUID -> Cmd Msg
fetchServerProjectCmd projectId =
    ProjectSync.fetchServerProject (projectIdString projectId)


fetchServerProjectVersionCmd : UUID -> Cmd Msg
fetchServerProjectVersionCmd projectId =
    ProjectSync.fetchServerProjectVersion (projectIdString projectId)


subscribeProjectCmd : UUID -> Cmd Msg
subscribeProjectCmd projectId =
    ProjectSync.subscribeProject (projectIdString projectId)


unsubscribeProjectCmd : UUID -> Cmd Msg
unsubscribeProjectCmd projectId =
    ProjectSync.unsubscribeProject (projectIdString projectId)


activeSyncedProjectId : Page -> Maybe UUID
activeSyncedProjectId page =
    case page of
        TodoGraph todoGraphModel ->
            if todoGraphModel.project.sync then
                Just todoGraphModel.project.id

            else
                Nothing

        _ ->
            Nothing


unsubscribeCurrentProjectExcept : Maybe UUID -> Model -> Cmd Msg
unsubscribeCurrentProjectExcept nextProjectId model =
    case activeSyncedProjectId model.page of
        Just projectId ->
            if Just projectId == nextProjectId then
                Cmd.none

            else
                unsubscribeProjectCmd projectId

        Nothing ->
            Cmd.none


projectOpenSyncCmd : Project.Project -> Cmd Msg
projectOpenSyncCmd project =
    if project.sync then
        Cmd.batch
            [ fetchServerProjectVersionCmd project.id
            , subscribeProjectCmd project.id
            ]

    else
        Cmd.none


updatedAtMillisDecoder : Decode.Decoder Time.Posix
updatedAtMillisDecoder =
    Decode.int
        |> Decode.map Time.millisToPosix


type alias ServerProjectEnvelope =
    { projectId : UUID
    , updatedAt : Time.Posix
    , payload : Project.Project
    }


serverProjectEnvelopeDecoder : Decode.Decoder ServerProjectEnvelope
serverProjectEnvelopeDecoder =
    Decode.map3 ServerProjectEnvelope
        (Decode.field "projectId" Project.uuidDecoder)
        (Decode.field "updatedAt" updatedAtMillisDecoder)
        (Decode.field "payload" Project.projectDecoder)


type alias ServerProjectVersion =
    { projectId : UUID
    , exists : Bool
    , updatedAt : Maybe Time.Posix
    }


serverProjectVersionDecoder : Decode.Decoder ServerProjectVersion
serverProjectVersionDecoder =
    Decode.map3 ServerProjectVersion
        (Decode.field "projectId" Project.uuidDecoder)
        (Decode.field "exists" Decode.bool)
        (Decode.maybe (Decode.field "updatedAt" updatedAtMillisDecoder))


type alias ServerProjectCheck =
    { projectId : UUID
    , exists : Bool
    , hasUpdate : Bool
    , serverUpdatedAt : Maybe Time.Posix
    }


serverProjectCheckDecoder : Decode.Decoder ServerProjectCheck
serverProjectCheckDecoder =
    Decode.map4 ServerProjectCheck
        (Decode.field "projectId" Project.uuidDecoder)
        (Decode.field "exists" Decode.bool)
        (Decode.field "hasUpdate" Decode.bool)
        (Decode.maybe (Decode.field "serverUpdatedAt" updatedAtMillisDecoder))


serverProjectChecksDecoder : Decode.Decoder (List ServerProjectCheck)
serverProjectChecksDecoder =
    Decode.list serverProjectCheckDecoder


type alias ServerSaveRejectedPayload =
    { projectId : Maybe UUID
    , reason : String
    , serverUpdatedAt : Maybe Time.Posix
    }


serverProjectSaveRejectedDecoder : Decode.Decoder ServerSaveRejectedPayload
serverProjectSaveRejectedDecoder =
    Decode.map3 ServerSaveRejectedPayload
        (Decode.oneOf
            [ Decode.field "projectId" (Decode.nullable Project.uuidDecoder)
            , Decode.succeed Nothing
            ]
        )
        (Decode.field "reason" Decode.string)
        (Decode.maybe (Decode.field "serverUpdatedAt" updatedAtMillisDecoder))


type alias ServerUpdatedPayload =
    { projectId : UUID
    , updatedAt : Time.Posix
    }


serverProjectUpdatedDecoder : Decode.Decoder ServerUpdatedPayload
serverProjectUpdatedDecoder =
    Decode.map2 ServerUpdatedPayload
        (Decode.field "projectId" Project.uuidDecoder)
        (Decode.field "updatedAt" updatedAtMillisDecoder)


type alias SyncFailure =
    { operation : String
    , projectId : Maybe UUID
    , message : String
    , status : Maybe Int
    }


syncFailureDecoder : Decode.Decoder SyncFailure
syncFailureDecoder =
    Decode.map4 SyncFailure
        (Decode.field "operation" Decode.string)
        (Decode.oneOf
            [ Decode.field "projectId" (Decode.nullable Project.uuidDecoder)
            , Decode.succeed Nothing
            ]
        )
        (Decode.field "message" Decode.string)
        (Decode.oneOf
            [ Decode.field "status" (Decode.nullable Decode.int)
            , Decode.succeed Nothing
            ]
        )


scheduleProjectSave : UUID -> Cmd Msg
scheduleProjectSave projectId =
    Time.now
        |> Task.perform (ProjectTimestampedForSave projectId)


generateFirstProject : Cmd Msg
generateFirstProject =
    Random.generate
        (\( projectId, nodeId ) -> FirstProjectGenerated projectId nodeId)
        (Random.map2 Tuple.pair UUID.generator UUID.generator)


gettingStartedProject : UUID -> UUID -> Project.Project
gettingStartedProject projectId nodeId =
    let
        project =
            Project.initialProject projectId nodeId
    in
    { project | name = Just "Getting Started" }


renameStoredProject : UUID -> Maybe String -> Project.Project -> Project.Project
renameStoredProject projectId name project =
    if project.id == projectId then
        { project | name = name }

    else
        project


cloneProject : UUID -> Project.Project -> Project.Project
cloneProject cloneId project =
    { project
        | id = cloneId
        , name = Just (projectDisplayName project ++ " (cloned)")
        , sync = False
    }


projectDisplayName : Project.Project -> String
projectDisplayName project =
    Maybe.withDefault
        ("Unnamed project " ++ UUID.toString project.id)
        project.name


cmdWithStorageSave : Cmd Msg -> Maybe Project.Project -> Cmd Msg
cmdWithStorageSave pageCmd maybeProject =
    case maybeProject of
        Just project ->
            Cmd.batch [ pageCmd, scheduleProjectSave project.id ]

        Nothing ->
            pageCmd


replaceCurrentPageProject : Project.Project -> Page -> Page
replaceCurrentPageProject project page =
    case page of
        TodoGraph todoGraphModel ->
            if todoGraphModel.project.id == project.id then
                TodoGraph { todoGraphModel | project = project }

            else
                page

        _ ->
            page


posixMillis : Time.Posix -> Int
posixMillis posix =
    Time.posixToMillis posix


posixGreaterThan : Time.Posix -> Time.Posix -> Bool
posixGreaterThan left right =
    posixMillis left > posixMillis right


posixLessThan : Time.Posix -> Time.Posix -> Bool
posixLessThan left right =
    posixMillis left < posixMillis right


serverProjectFromEnvelope : ServerProjectEnvelope -> Project.Project
serverProjectFromEnvelope envelope =
    envelope.payload
        |> Project.setSync True
        |> (\project -> { project | updatedAt = envelope.updatedAt })


syncCommandForServerVersion : ServerProjectVersion -> Model -> Cmd Msg
syncCommandForServerVersion version model =
    case findProject version.projectId model.projects of
        Just localProject ->
            if localProject.sync then
                case version.updatedAt of
                    Just serverUpdatedAt ->
                        if posixGreaterThan serverUpdatedAt localProject.updatedAt then
                            fetchServerProjectCmd version.projectId

                        else if posixLessThan serverUpdatedAt localProject.updatedAt then
                            saveServerProjectCmd localProject

                        else
                            Cmd.none

                    Nothing ->
                        saveServerProjectCmd localProject

            else
                Cmd.none

        Nothing ->
            Cmd.none


syncCommandForServerCheck : ServerProjectCheck -> Model -> Cmd Msg
syncCommandForServerCheck check model =
    case findProject check.projectId model.projects of
        Just localProject ->
            if localProject.sync then
                case ( check.exists, check.serverUpdatedAt ) of
                    ( False, _ ) ->
                        saveServerProjectCmd localProject

                    ( True, Just serverUpdatedAt ) ->
                        if posixGreaterThan serverUpdatedAt localProject.updatedAt then
                            fetchServerProjectCmd check.projectId

                        else if posixLessThan serverUpdatedAt localProject.updatedAt then
                            saveServerProjectCmd localProject

                        else
                            Cmd.none

                    _ ->
                        Cmd.none

            else
                Cmd.none

        Nothing ->
            Cmd.none


handleServerProjectLoaded : ServerProjectEnvelope -> Model -> ( Model, Cmd Msg )
handleServerProjectLoaded envelope model =
    let
        serverProject =
            serverProjectFromEnvelope envelope

        nextProjects =
            upsertProject serverProject model.projects

        openLoadedProject =
            case model.page of
                ProjectLoading loadingProjectId ->
                    loadingProjectId == serverProject.id

                ProjectLoadFailed failedProjectId _ ->
                    failedProjectId == serverProject.id

                TodoGraph todoGraphModel ->
                    todoGraphModel.project.id == serverProject.id

                _ ->
                    False

        ( nextPage, pageCmd ) =
            if openLoadedProject then
                let
                    ( todoGraphModel, todoGraphCmd ) =
                        initTodoGraph serverProject
                in
                ( TodoGraph todoGraphModel
                , Cmd.map TodoGraphMsg todoGraphCmd
                )

            else
                ( model.page, Cmd.none )
    in
    ( { model
        | projects = nextProjects
        , projectSelector = ProjectSelectorPage.init nextProjects
        , page = nextPage
      }
    , Cmd.batch
        [ saveProjectCmd serverProject
        , pageCmd
        , if openLoadedProject then
            subscribeProjectCmd serverProject.id

          else
            Cmd.none
        ]
    )


handleServerProjectUpdated : ServerUpdatedPayload -> Model -> ( Model, Cmd Msg )
handleServerProjectUpdated serverUpdate model =
    case findProject serverUpdate.projectId model.projects of
        Just localProject ->
            if localProject.sync && posixGreaterThan serverUpdate.updatedAt localProject.updatedAt then
                ( model, fetchServerProjectCmd serverUpdate.projectId )

            else
                ( model, Cmd.none )

        Nothing ->
            ( model, Cmd.none )


handleServerProjectSaveRejected : ServerSaveRejectedPayload -> Model -> ( Model, Cmd Msg )
handleServerProjectSaveRejected rejection model =
    case ( rejection.reason, rejection.projectId, rejection.serverUpdatedAt ) of
        ( "stale_update", Just projectId, Just serverUpdatedAt ) ->
            case findProject projectId model.projects of
                Just localProject ->
                    if posixGreaterThan serverUpdatedAt localProject.updatedAt then
                        ( model, fetchServerProjectCmd projectId )

                    else
                        ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


handleSyncFailure : SyncFailure -> Model -> ( Model, Cmd Msg )
handleSyncFailure failure model =
    case ( failure.operation, failure.projectId, model.page ) of
        ( "fetchProject", Just projectId, ProjectLoading loadingProjectId ) ->
            if projectId == loadingProjectId then
                let
                    message =
                        case failure.status of
                            Just 404 ->
                                "Project was not found on this device or on the server."

                            _ ->
                                "Project is not available locally, and the server could not be reached."
                in
                ( { model | page = ProjectLoadFailed projectId message }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


timestampProjectForSave : UUID -> Time.Posix -> Model -> ( Model, Cmd Msg )
timestampProjectForSave projectId updatedAt model =
    case findProject projectId model.projects of
        Just project ->
            let
                stampedProject =
                    Project.touch updatedAt project
            in
            ( { model
                | projects = upsertProject stampedProject model.projects
                , page = replaceCurrentPageProject stampedProject model.page
              }
            , Cmd.batch
                [ saveProjectCmd stampedProject
                , debounceSaveSyncedProjectCmd stampedProject
                ]
            )

        Nothing ->
            ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( ToggleDesktopSidebar, _ ) ->
            ( { model | desktopSidebarVisible = not model.desktopSidebarVisible }
            , Cmd.none
            )

        ( ToggleMobileSidebar, _ ) ->
            ( { model | mobileSidebarOpen = not model.mobileSidebarOpen }
            , Cmd.none
            )

        ( CloseMobileSidebar, _ ) ->
            ( { model | mobileSidebarOpen = False }
            , Cmd.none
            )

        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        ( ChangedUrl url, AppInit _ ) ->
            ( { model | route = Route.fromUrl url }
            , Cmd.none
            )

        ( ChangedUrl url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( ProjectSelectorMsg projectSelectorMsg, _ ) ->
            let
                ( updatedSelector, newCmd ) =
                    ProjectSelectorPage.update projectSelectorMsg model.projectSelector

                pageCmd =
                    Cmd.map ProjectSelectorMsg newCmd

                newModel =
                    { model | projectSelector = updatedSelector }
            in
            case projectSelectorMsg of
                ProjectSelectorPage.OpenProject projectIdText ->
                    ( { newModel | mobileSidebarOpen = False }
                    , Cmd.batch
                        [ pageCmd
                        , Nav.pushUrl model.key ("/p/" ++ projectIdText)
                        ]
                    )

                ProjectSelectorPage.NewProjectGenerated projectId ->
                    let
                        newProject =
                            Project.initialProject projectId projectId

                        nextProjects =
                            newProject :: model.projects

                        ( todoGraphModel, todoGraphCmd ) =
                            initTodoGraph newProject
                    in
                    ( { newModel
                        | projects = nextProjects
                        , page = TodoGraph todoGraphModel
                        , route = Just (Route.Project projectId)
                        , mobileSidebarOpen = False
                      }
                    , Cmd.batch
                        [ pageCmd
                        , Cmd.map TodoGraphMsg todoGraphCmd
                        , unsubscribeCurrentProjectExcept Nothing model
                        , Nav.pushUrl model.key ("/p/" ++ UUID.toString projectId)
                        , scheduleProjectSave newProject.id
                        ]
                    )

                ProjectSelectorPage.CloneProject sourceProjectId ->
                    ( newModel
                    , Cmd.batch
                        [ pageCmd
                        , Random.generate (ProjectCloned sourceProjectId) UUID.generator
                        ]
                    )

                ProjectSelectorPage.SaveProjectName projectId ->
                    let
                        nextName =
                            updatedSelector.projects
                                |> List.filter (.id >> (==) projectId)
                                |> List.head
                                |> Maybe.andThen .name

                        nextProjects =
                            model.projects
                                |> List.map (renameStoredProject projectId nextName)

                        maybeChangedProject =
                            findProject projectId nextProjects
                                |> Maybe.andThen
                                    (\nextProject ->
                                        if findProject projectId model.projects == Just nextProject then
                                            Nothing

                                        else
                                            Just nextProject
                                    )

                        nextPage =
                            maybeChangedProject
                                |> Maybe.map (\changedProject -> replaceCurrentPageProject changedProject model.page)
                                |> Maybe.withDefault model.page
                    in
                    ( { newModel
                        | projects = nextProjects
                        , page = nextPage
                      }
                    , cmdWithStorageSave pageCmd maybeChangedProject
                    )

                ProjectSelectorPage.ToggleProjectSync projectId sync ->
                    let
                        nextProjects =
                            model.projects
                                |> List.map
                                    (\project ->
                                        if project.id == projectId then
                                            Project.setSync sync project

                                        else
                                            project
                                    )

                        maybeChangedProject =
                            findProject projectId nextProjects

                        nextPage =
                            maybeChangedProject
                                |> Maybe.map (\changedProject -> replaceCurrentPageProject changedProject model.page)
                                |> Maybe.withDefault model.page

                        syncCmd =
                            case maybeChangedProject of
                                Just changedProject ->
                                    if sync then
                                        Cmd.batch
                                            [ saveServerProjectCmd changedProject
                                            , if activeProjectId model.page == Just projectId then
                                                subscribeProjectCmd projectId

                                              else
                                                Cmd.none
                                            ]

                                    else
                                        unsubscribeProjectCmd projectId

                                Nothing ->
                                    Cmd.none
                    in
                    ( { newModel
                        | projects = nextProjects
                        , page = nextPage
                      }
                    , Cmd.batch
                        [ pageCmd
                        , maybeChangedProject
                            |> Maybe.map saveProjectCmd
                            |> Maybe.withDefault Cmd.none
                        , syncCmd
                        ]
                    )

                ProjectSelectorPage.ConfirmProjectDelete projectId ->
                    let
                        nextProjects =
                            model.projects
                                |> List.filter (.id >> (/=) projectId)

                        deletedActiveProject =
                            activeProjectId model.page == Just projectId

                        deletedSyncedActiveProject =
                            activeSyncedProjectId model.page == Just projectId

                        nextModel =
                            if deletedActiveProject then
                                { newModel
                                    | projects = nextProjects
                                    , page = NoProjectSelected
                                    , route = Just Route.ProjectSelector
                                    , mobileSidebarOpen = False
                                }

                            else
                                { newModel | projects = nextProjects }

                        routeCmd =
                            if deletedActiveProject then
                                Nav.replaceUrl model.key "/"

                            else
                                Cmd.none
                    in
                    ( nextModel
                    , Cmd.batch
                        [ pageCmd
                        , deleteProjectCmd projectId
                        , routeCmd
                        , if deletedSyncedActiveProject then
                            unsubscribeProjectCmd projectId

                          else
                            Cmd.none
                        ]
                    )

                _ ->
                    ( newModel, pageCmd )

        ( TodoGraphMsg todoGraphMsg, TodoGraph page ) ->
            let
                ( updatedPage, newCmd ) =
                    TodoGraphPage.update todoGraphMsg page

                projectChanged =
                    updatedPage.project /= page.project

                nextProjects =
                    if projectChanged then
                        upsertProject updatedPage.project model.projects

                    else
                        model.projects

                newModel =
                    { model
                        | page = TodoGraph updatedPage
                        , projects = nextProjects
                    }

                pageCmd =
                    Cmd.map TodoGraphMsg newCmd
            in
            if projectChanged then
                ( newModel
                , Cmd.batch [ pageCmd, scheduleProjectSave updatedPage.project.id ]
                )

            else
                ( newModel, pageCmd )

        ( ProjectsLoaded value, _ ) ->
            case Decode.decodeValue Project.projectsDecoder value of
                Ok storedProjects ->
                    let
                        ( routedModel, routeCmd ) =
                            changeRouteTo model.route
                                { model
                                    | projects = storedProjects
                                    , projectSelector = ProjectSelectorPage.init storedProjects
                                }
                    in
                    ( routedModel
                    , Cmd.batch
                        [ routeCmd
                        , checkSyncedProjectsCmd storedProjects
                        ]
                    )

                Err error ->
                    ( { model | storageError = Just (Decode.errorToString error) }
                    , generateFirstProject
                    )

        ( StorageFailed error, _ ) ->
            case model.page of
                AppInit _ ->
                    ( { model | storageError = Just error }
                    , generateFirstProject
                    )

                _ ->
                    ( { model | storageError = Just error }
                    , Cmd.none
                    )

        ( FirstProjectGenerated projectId nodeId, _ ) ->
            let
                firstProject =
                    gettingStartedProject projectId nodeId

                modelWithProject =
                    { model
                        | projects = [ firstProject ]
                        , projectSelector = ProjectSelectorPage.init [ firstProject ]
                    }

                ( routedModel, routeCmd ) =
                    changeRouteTo model.route modelWithProject
            in
            ( routedModel
            , Cmd.batch [ routeCmd, scheduleProjectSave firstProject.id ]
            )

        ( ProjectCloned sourceProjectId cloneId, _ ) ->
            case findProject sourceProjectId model.projects of
                Just sourceProject ->
                    let
                        clonedProject =
                            cloneProject cloneId sourceProject

                        nextProjects =
                            clonedProject :: model.projects

                        ( updatedSelector, selectorCmd ) =
                            ProjectSelectorPage.update
                                (ProjectSelectorPage.ProjectCloned clonedProject)
                                model.projectSelector

                        ( todoGraphModel, todoGraphCmd ) =
                            initTodoGraph clonedProject
                    in
                    ( { model
                        | projects = nextProjects
                        , projectSelector = updatedSelector
                        , page = TodoGraph todoGraphModel
                        , route = Just (Route.Project cloneId)
                        , mobileSidebarOpen = False
                      }
                    , Cmd.batch
                        [ Cmd.map ProjectSelectorMsg selectorCmd
                        , Cmd.map TodoGraphMsg todoGraphCmd
                        , unsubscribeCurrentProjectExcept Nothing model
                        , Nav.pushUrl model.key ("/p/" ++ UUID.toString cloneId)
                        , scheduleProjectSave clonedProject.id
                        ]
                    )

                Nothing ->
                    ( model, Cmd.none )

        ( ServerProjectsChecked value, _ ) ->
            case Decode.decodeValue serverProjectChecksDecoder value of
                Ok checks ->
                    ( model
                    , checks
                        |> List.map (\check -> syncCommandForServerCheck check model)
                        |> Cmd.batch
                    )

                Err _ ->
                    ( model, Cmd.none )

        ( ServerProjectVersionLoaded value, _ ) ->
            case Decode.decodeValue serverProjectVersionDecoder value of
                Ok version ->
                    ( model, syncCommandForServerVersion version model )

                Err _ ->
                    ( model, Cmd.none )

        ( ServerProjectLoaded value, _ ) ->
            case Decode.decodeValue serverProjectEnvelopeDecoder value of
                Ok envelope ->
                    handleServerProjectLoaded envelope model

                Err _ ->
                    ( model, Cmd.none )

        ( ServerProjectSaveAccepted _, _ ) ->
            ( model, Cmd.none )

        ( ServerProjectSaveRejected value, _ ) ->
            case Decode.decodeValue serverProjectSaveRejectedDecoder value of
                Ok rejection ->
                    handleServerProjectSaveRejected rejection model

                Err _ ->
                    ( model, Cmd.none )

        ( ServerProjectUpdated value, _ ) ->
            case Decode.decodeValue serverProjectUpdatedDecoder value of
                Ok serverUpdate ->
                    handleServerProjectUpdated serverUpdate model

                Err _ ->
                    ( model, Cmd.none )

        ( SyncFailed value, _ ) ->
            case Decode.decodeValue syncFailureDecoder value of
                Ok failure ->
                    handleSyncFailure failure model

                Err _ ->
                    ( model, Cmd.none )

        ( ProjectTimestampedForSave projectId updatedAt, _ ) ->
            timestampProjectForSave projectId updatedAt model

        _ ->
            ( model, Cmd.none )


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        }
