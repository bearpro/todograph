module Control.TodoGraphItem exposing (..)

import Html exposing (Html, button, div, input, label, text)
import Html.Attributes exposing (checked, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import Platform.Cmd as Cmd


type TextEditState
    = NotEditing
    | Editing String


type alias TextItemModel =
    { text : String
    , status : Bool
    , editState : TextEditState
    }


type Model
    = Text TextItemModel


type Msg
    = CheckboxToggle Bool
    | StartTextEdit
    | SetText String
    | DraftChanged String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( CheckboxToggle newValue, Text textModel ) ->
            ( Text { textModel | status = newValue }
            , Cmd.none
            )

        ( StartTextEdit, Text textModel ) ->
            ( Text { textModel | editState = Editing textModel.text }
            , Cmd.none
            )

        ( SetText newText, Text textModel ) ->
            ( Text { textModel | text = newText, editState = NotEditing }
            , Cmd.none
            )

        ( DraftChanged newDraftText, Text textModel ) ->
            case textModel.editState of
                Editing _ ->
                    let
                        newModel =
                            Text { textModel | editState = Editing newDraftText }
                    in
                    ( newModel, Cmd.none )

                NotEditing ->
                    ( model, Cmd.none )


viewTextItem : TextItemModel -> Html Msg
viewTextItem model =
    div
        []
        [ label
            []
            [ input
                [ type_ "checkbox"
                , checked model.status
                , onCheck CheckboxToggle
                ]
                []
            , case model.editState of
                NotEditing ->
                    text model.text

                Editing draft ->
                    input
                        [ value draft
                        , onInput DraftChanged
                        ]
                        []
            , case model.editState of
                NotEditing ->
                    button
                        [ onClick StartTextEdit ]
                        [ text "✍️" ]

                Editing draft ->
                    button
                        [ onClick (SetText draft) ]
                        [ text "💾" ]
            ]
        ]


view : Model -> Html Msg
view model =
    case model of
        Text textModel ->
            viewTextItem textModel
