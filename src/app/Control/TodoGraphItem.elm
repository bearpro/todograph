module Control.TodoGraphItem exposing (..)

import Browser.Dom as Dom
import Domain.Project as Project
import Html exposing (Attribute, Html, button, div, input, span, text)
import Html.Attributes exposing (autofocus, checked, class, id, style, type_, value)
import Html.Events exposing (on, onBlur, onCheck, onClick, onInput)
import Json.Decode as Decode
import Platform.Cmd as Cmd
import Task
import UUID exposing (UUID)


type TextEditState
    = NotEditing
    | Editing String


type alias Model =
    { node : Project.Node
    , textEditState : TextEditState
    }


type alias ViewOptions =
    { hideButtons : Bool
    }


type Msg
    = CheckboxToggle Bool
    | StartTextEdit
    | TextInputFocused (Result Dom.Error ())
    | StopTextEdit
    | DraftChanged String
    | ToggleTimerRunning


fromNode : Project.Node -> Model
fromNode node =
    { node = node
    , textEditState = NotEditing
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CheckboxToggle newValue ->
            ( { model | node = updateNodeStatus newValue model.node }
            , Cmd.none
            )

        StartTextEdit ->
            ( { model | textEditState = Editing model.node.text }
            , Dom.focus (textInputId model.node.id)
                |> Task.attempt TextInputFocused
            )

        TextInputFocused _ ->
            ( model, Cmd.none )

        StopTextEdit ->
            ( { model | textEditState = NotEditing }
            , Cmd.none
            )

        DraftChanged newDraftText ->
            case model.textEditState of
                Editing _ ->
                    ( { model
                        | node = updateNodeText newDraftText model.node
                        , textEditState = Editing newDraftText
                      }
                    , Cmd.none
                    )

                NotEditing ->
                    ( model, Cmd.none )

        ToggleTimerRunning ->
            ( { model | node = toggleTimer model.node }
            , Cmd.none
            )


updateNodeStatus : Bool -> Project.Node -> Project.Node
updateNodeStatus status node =
    { node | status = status }


updateNodeText : String -> Project.Node -> Project.Node
updateNodeText nodeText node =
    { node | text = nodeText }


toggleTimer : Project.Node -> Project.Node
toggleTimer node =
    { node
        | timer =
            node.timer
                |> Maybe.map (\timer -> { timer | running = not timer.running })
    }


viewText : Project.Node -> TextEditState -> Html Msg
viewText node editState =
    case editState of
        NotEditing ->
            span
                [ class "flex-grow-1"
                , onClick StartTextEdit
                , style "cursor" "text"
                , style "min-width" "0"
                ]
                [ text node.text ]

        Editing draft ->
            input
                [ value draft
                , id (textInputId node.id)
                , onInput DraftChanged
                , onBlur StopTextEdit
                , onEnter StopTextEdit
                , autofocus True
                , class "form-control form-control-sm flex-grow-1"
                ]
                []


textInputId : UUID -> String
textInputId nodeId =
    "todo-graph-node-text-" ++ UUID.toString nodeId


viewTimer : Bool -> Maybe Project.Timer -> Html Msg
viewTimer hideButtons maybeTimer =
    case maybeTimer of
        Just timer ->
            div [ class "d-flex align-items-center gap-2 ms-auto" ]
                (if hideButtons then
                    [ span [ class "text-muted small" ] [ text (formatSeconds timer.elapsedSeconds) ] ]

                 else
                    [ button
                        [ onClick ToggleTimerRunning
                        , class
                            (if timer.running then
                                "btn btn-sm btn-success"

                             else
                                "btn btn-sm btn-outline-secondary"
                            )
                        ]
                        [ text
                            (if timer.running then
                                "pause"

                             else
                                "start"
                            )
                        ]
                    , span [ class "text-muted small" ] [ text (formatSeconds timer.elapsedSeconds) ]
                    ]
                )

        Nothing ->
            text ""


formatSeconds : Int -> String
formatSeconds seconds =
    let
        minutes =
            seconds // 60

        remainder =
            modBy 60 seconds

        paddedRemainder =
            if remainder < 10 then
                "0" ++ String.fromInt remainder

            else
                String.fromInt remainder
    in
    String.fromInt minutes ++ ":" ++ paddedRemainder


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


view : ViewOptions -> Model -> Html Msg
view options model =
    div
        [ class "d-flex align-items-center gap-3 px-3 py-2"
        , style "min-height" "58px"
        , style "background" "#f5f5f5"
        , style "border" "1px solid #ececec"
        , style "box-sizing" "border-box"
        ]
        [ input
            [ type_ "checkbox"
            , checked model.node.status
            , onCheck CheckboxToggle
            , style "width" "24px"
            , style "height" "24px"
            , style "flex" "0 0 auto"
            ]
            []
        , viewText model.node model.textEditState
        , viewTimer options.hideButtons model.node.timer
        ]
