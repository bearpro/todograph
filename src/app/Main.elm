module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html)
import Page.AppInit as AppInitPage
import Page.ProjectSelector as ProjectSelectorPage
import Page.TodoGraph as TodoGraphPage
import Url exposing (Url)


type Page
    = AppInit AppInitPage.Model
    | TodoGraph TodoGraphPage.Model
    | ProjectSelector ProjectSelectorPage.Model


type alias Model =
    { page : Page }


type Msg
    = ClickedLink Browser.UrlRequest
    | ChangedUrl Url
    | AppInitMsg AppInitPage.Msg
    | ProjectSelectorMsg ProjectSelectorPage.Msg
    | TodoGraphMsg TodoGraphPage.Msg


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init () _ _ =
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
            { page = page }
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
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

        ( ProjectSelectorMsg (ProjectSelectorPage.NewProjectGenerated id), _ ) ->
            let
                newProject =
                    { id = id
                    , name = "Stub"
                    }
            in
            let
                newModel =
                    { model | page = TodoGraph (TodoGraphPage.init newProject) }
            in
            ( newModel, Cmd.none )

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
