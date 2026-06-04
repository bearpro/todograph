module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Control.Navbar
import Domain.Project as Project
import Html exposing (Html)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Page.AppInit as AppInitPage
import Page.ProjectSelector as ProjectSelectorPage
import Page.TodoGraph as TodoGraphPage
import Ports.ProjectStorage as ProjectStorage
import Random
import Route exposing (Route)
import Task
import Time
import UUID exposing (UUID)
import Url exposing (Url)


type Page
    = AppInit AppInitPage.Model
    | TodoGraph TodoGraphPage.Model
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
    | ProjectTimestampedForSave UUID Time.Posix
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
    { title = "TodoGraph | " ++ workspace.title
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
            , Nav.replaceUrl model.key "/"
            )

        Just Route.ProjectSelector ->
            ( { model
                | page = NoProjectSelected
                , projectSelector = ProjectSelectorPage.init model.projects
                , route = maybeRoute
                , mobileSidebarOpen = False
              }
            , Cmd.none
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
                        , projectSelector = ProjectSelectorPage.init model.projects
                        , route = maybeRoute
                        , mobileSidebarOpen = False
                      }
                    , Cmd.map TodoGraphMsg cmd
                    )

                Nothing ->
                    ( { model
                        | page = NoProjectSelected
                        , projectSelector = ProjectSelectorPage.init model.projects
                        , route = Just Route.ProjectSelector
                        , mobileSidebarOpen = False
                      }
                    , Nav.replaceUrl model.key "/"
                    )


initTodoGraph : Project.Project -> ( TodoGraphPage.Model, Cmd TodoGraphPage.Msg )
initTodoGraph project =
    TodoGraphPage.init
        { project = project
        , nodeUiStates = []
        , joinDrag = Nothing
        , now = Nothing
        , openNewMenu = Nothing
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
            , saveProjectCmd stampedProject
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
                ProjectSelectorPage.OpenProject _ ->
                    ( { newModel | mobileSidebarOpen = False }, pageCmd )

                ProjectSelectorPage.NewProjectGenerated projectId ->
                    let
                        newProject =
                            Project.initialProject projectId projectId

                        nextProjects =
                            newProject :: model.projects
                    in
                    ( { newModel | projects = nextProjects }
                    , Cmd.batch [ pageCmd, scheduleProjectSave newProject.id ]
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
                    if List.isEmpty storedProjects then
                        ( model, generateFirstProject )

                    else
                        changeRouteTo model.route { model | projects = storedProjects }

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
                    { model | projects = [ firstProject ] }

                ( routedModel, routeCmd ) =
                    changeRouteTo model.route modelWithProject
            in
            ( routedModel
            , Cmd.batch [ routeCmd, scheduleProjectSave firstProject.id ]
            )

        ( ProjectTimestampedForSave projectId updatedAt, _ ) ->
            timestampProjectForSave projectId updatedAt model

        _ ->
            Debug.log "Unexpected root update"
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
