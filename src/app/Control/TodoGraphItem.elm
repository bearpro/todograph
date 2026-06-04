module Control.TodoGraphItem exposing (..)

import Browser.Dom as Dom
import Domain.Project as Project
import Html exposing (Attribute, Html, button, div, input, span, text)
import Html.Attributes exposing (autofocus, checked, class, id, style, type_, value)
import Html.Events exposing (on, onBlur, onCheck, onClick, onInput)
import Json.Decode as Decode
import Platform.Cmd as Cmd
import Task
import Time
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
    , now : Maybe Time.Posix
    }


type Msg
    = CheckboxToggle Bool
    | StartTextEdit
    | TextInputFocused (Result Dom.Error ())
    | StopTextEdit
    | DraftChanged String
    | StartTimer
    | TimerStarted Time.Posix
    | StopTimer
    | TimerStopped Time.Posix


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

        StartTimer ->
            ( model, Task.perform TimerStarted Time.now )

        TimerStarted now ->
            ( { model | node = startTimer now model.node }
            , Cmd.none
            )

        StopTimer ->
            ( model, Task.perform TimerStopped Time.now )

        TimerStopped now ->
            ( { model | node = stopTimer now model.node }
            , Cmd.none
            )


updateNodeStatus : Bool -> Project.Node -> Project.Node
updateNodeStatus status node =
    { node | status = status }


updateNodeText : String -> Project.Node -> Project.Node
updateNodeText nodeText node =
    { node | text = nodeText }


startTimer : Time.Posix -> Project.Node -> Project.Node
startTimer now node =
    { node
        | timer =
            node.timer
                |> Maybe.map (\_ -> Project.Started now)
    }


stopTimer : Time.Posix -> Project.Node -> Project.Node
stopTimer now node =
    { node
        | timer =
            node.timer
                |> Maybe.map
                    (\timer ->
                        case timer of
                            Project.Started startedAt ->
                                Project.Stopped (elapsedSeconds startedAt now)

                            Project.Stopped seconds ->
                                Project.Stopped seconds
                    )
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


viewTimer : ViewOptions -> Maybe Project.Timer -> Html Msg
viewTimer options maybeTimer =
    case maybeTimer of
        Just timer ->
            div [ class "d-flex align-items-center gap-2 ms-auto" ]
                (if options.hideButtons then
                    [ span [ class "text-muted small" ] [ text (formatSeconds (timerSeconds options.now timer)) ] ]

                 else
                    [ button
                        [ onClick (timerButtonMsg timer)
                        , class
                            (case timer of
                                Project.Started _ ->
                                    "btn btn-sm btn-success"

                                Project.Stopped seconds ->
                                    if seconds > 0 then
                                        "btn btn-sm btn-warning"

                                    else
                                        "btn btn-sm btn-outline-secondary"
                            )
                        ]
                        [ text (timerButtonLabel timer) ]
                    , span [ class "text-muted small" ] [ text (formatSeconds (timerSeconds options.now timer)) ]
                    ]
                )

        Nothing ->
            text ""


timerButtonMsg : Project.Timer -> Msg
timerButtonMsg timer =
    case timer of
        Project.Started _ ->
            StopTimer

        Project.Stopped _ ->
            StartTimer


timerButtonLabel : Project.Timer -> String
timerButtonLabel timer =
    case timer of
        Project.Started _ ->
            "Stop"

        Project.Stopped seconds ->
            if seconds > 0 then
                "Restart"

            else
                "Start"


timerSeconds : Maybe Time.Posix -> Project.Timer -> Int
timerSeconds maybeNow timer =
    case timer of
        Project.Started startedAt ->
            maybeNow
                |> Maybe.map (elapsedSeconds startedAt)
                |> Maybe.withDefault 0

        Project.Stopped seconds ->
            seconds


elapsedSeconds : Time.Posix -> Time.Posix -> Int
elapsedSeconds startedAt now =
    max 0 ((Time.posixToMillis now - Time.posixToMillis startedAt) // 1000)


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
        , viewTimer options model.node.timer
        ]
