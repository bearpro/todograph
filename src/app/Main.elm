module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Control.TodoGraphItem
import Html exposing (Html)
import Page.AppInit as AppInitPage
import Page.ProjectSelector as ProjectSelectorPage
import Page.TodoGraph as TodoGraphPage exposing (Msg(..))
import Route exposing (Route)
import Url exposing (Url)


type Page
    = AppInit AppInitPage.Model
    | TodoGraph TodoGraphPage.Model
    | ProjectSelector ProjectSelectorPage.Model


type alias Model =
    { page : Page
    , key : Nav.Key
    }


type Msg
    = ClickedLink Browser.UrlRequest
    | ChangedUrl Url
    | AppInitMsg AppInitPage.Msg
    | ProjectSelectorMsg ProjectSelectorPage.Msg
    | TodoGraphMsg TodoGraphPage.Msg


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init () _ navKey =
    let
        ( appInitModel, cmd ) =
            AppInitPage.init
    in
    let
        page =
            AppInit appInitModel
    in
    let
        model =
            { page = page, key = navKey }
    in
    ( model, Cmd.map AppInitMsg cmd )


mapDocument : (childMsg -> parentMsg) -> Browser.Document childMsg -> Browser.Document parentMsg
mapDocument toParent document =
    { title = "TodoGraph | " ++ document.title
    , body = List.map (Html.map toParent) document.body
    }


viewPage : Page -> Browser.Document Msg
viewPage page =
    case page of
        AppInit model ->
            mapDocument AppInitMsg (AppInitPage.view model)

        TodoGraph model ->
            mapDocument TodoGraphMsg (TodoGraphPage.view model)

        ProjectSelector model ->
            mapDocument ProjectSelectorMsg (ProjectSelectorPage.view model)


view : Model -> Browser.Document Msg
view model =
    viewPage model.page


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    case maybeRoute of
        Nothing ->
            ( { model | page = ProjectSelector ProjectSelectorPage.init }
            , Nav.replaceUrl model.key "/"
            )

        Just Route.ProjectSelector ->
            ( { model | page = ProjectSelector ProjectSelectorPage.init }
            , Cmd.none
            )

        Just (Route.Project projectId) ->
            let
                ( todoGraphModel, cmd ) =
                    TodoGraphPage.init
                        { id = projectId
                        , name = "Stub"
                        , graphItems =
                            [ Control.TodoGraphItem.Text
                                { text = "Stub"
                                , status = False
                                , editState = Control.TodoGraphItem.NotEditing
                                }
                            ]
                        }
            in
            ( { model | page = TodoGraph todoGraphModel }
            , Cmd.map TodoGraphMsg cmd
            )


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

        ( ChangedUrl url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( AppInitMsg AppInitPage.Loaded, _ ) ->
            let
                newModel =
                    { model | page = ProjectSelector ProjectSelectorPage.init }
            in
            ( newModel, Cmd.none )

        ( ProjectSelectorMsg projectSelectorMsg, ProjectSelector page ) ->
            let
                ( updatedPage, newCmd ) =
                    ProjectSelectorPage.update projectSelectorMsg page
            in
            let
                newModel =
                    { model | page = ProjectSelector updatedPage }
            in
            ( newModel, Cmd.map ProjectSelectorMsg newCmd )

        ( TodoGraphMsg todoGraphMsg, TodoGraph page ) ->
            let
                ( updatedPage, newCmd ) =
                    TodoGraphPage.update todoGraphMsg page
            in
            let
                newModel =
                    { model | page = TodoGraph updatedPage }
            in
            ( newModel, Cmd.map TodoGraphMsg newCmd )

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
