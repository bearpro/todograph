module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Control.Navbar
import Domain.Project as Project
import Html
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
    | ProjectSelector ProjectSelectorPage.Model


type alias Model =
    { page : Page
    , key : Nav.Key
    , projects : List Project.Project
    , route : Maybe Route
    , storageError : Maybe String
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


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init () url navKey =
    ( { page = AppInit AppInitPage.init
      , key = navKey
      , projects = []
      , route = Route.fromUrl url
      , storageError = Nothing
      }
    , ProjectStorage.loadProjects ()
    )


currentRoute : Page -> Route
currentRoute page =
    case page of
        AppInit _ ->
            Route.ProjectSelector

        ProjectSelector _ ->
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


mapDocument : Page -> (childMsg -> parentMsg) -> Browser.Document childMsg -> Browser.Document parentMsg
mapDocument page toParent document =
    { title = "TodoGraph | " ++ document.title
    , body =
        Control.Navbar.view
            { currentPage = currentRoute page
            , currentProjectName = currentProjectName page
            }
            :: List.map (Html.map toParent) document.body
    }


viewPage : Page -> Browser.Document Msg
viewPage page =
    case page of
        AppInit model ->
            mapDocument page identity (AppInitPage.view model)

        TodoGraph model ->
            mapDocument page TodoGraphMsg (TodoGraphPage.view model)

        ProjectSelector model ->
            mapDocument page ProjectSelectorMsg (ProjectSelectorPage.view model)


view : Model -> Browser.Document Msg
view model =
    viewPage model.page


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
                | page = ProjectSelector (ProjectSelectorPage.init model.projects)
                , route = maybeRoute
              }
            , Nav.replaceUrl model.key "/"
            )

        Just Route.ProjectSelector ->
            ( { model
                | page = ProjectSelector (ProjectSelectorPage.init model.projects)
                , route = maybeRoute
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
                        , route = maybeRoute
                      }
                    , Cmd.map TodoGraphMsg cmd
                    )

                Nothing ->
                    ( { model
                        | page = ProjectSelector (ProjectSelectorPage.init model.projects)
                        , route = Just Route.ProjectSelector
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

        ( ProjectSelectorMsg projectSelectorMsg, ProjectSelector page ) ->
            let
                ( updatedPage, newCmd ) =
                    ProjectSelectorPage.update projectSelectorMsg page

                pageCmd =
                    Cmd.map ProjectSelectorMsg newCmd

                newModel =
                    { model | page = ProjectSelector updatedPage }
            in
            case projectSelectorMsg of
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
                            updatedPage.projects
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
                    in
                    ( { newModel | projects = nextProjects }
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
