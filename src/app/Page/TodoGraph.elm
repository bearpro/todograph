module Page.TodoGraph exposing (..)

import Browser exposing (Document)
import Control.TodoGraphItem
import Html exposing (Html, div)
import Platform.Cmd as Cmd
import UUID exposing (UUID)


type alias Model =
    { id : UUID
    , name : Maybe String
    , graphItems : List Control.TodoGraphItem.Model
    }


type Msg
    = GraphItemMsg Int Control.TodoGraphItem.Msg


init : Model -> ( Model, Cmd Msg )
init model =
    ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GraphItemMsg index graphItemMsg ->
            let
                updateItem =
                    \i ->
                        \item ->
                            if i == index then
                                Control.TodoGraphItem.update
                                    graphItemMsg
                                    item

                            else
                                ( item, Cmd.none )
            in
            let
                ( newItems, commands ) =
                    model.graphItems
                        |> List.indexedMap updateItem
                        |> List.unzip
            in
            let
                command =
                    case commands of
                        single :: [] ->
                            Cmd.map (GraphItemMsg index) single

                        [] ->
                            Cmd.none

                        _ ->
                            Cmd.none
            in
            Debug.log "Update"
                ( { model | graphItems = newItems }
                , command
                )


viewGraphItems : List Control.TodoGraphItem.Model -> Html Msg
viewGraphItems items =
    let
        viewItem =
            \i ->
                \item ->
                    let
                        itemHtml =
                            Control.TodoGraphItem.view item
                    in
                    Html.map (GraphItemMsg i) itemHtml
    in
    div
        []
        (items
            |> List.indexedMap viewItem
        )


view : Model -> Document Msg
view model =
    { title = Maybe.withDefault ("Project" ++ UUID.toString model.id) model.name
    , body =
        [ div [] []
        , viewGraphItems model.graphItems
        ]
    }
