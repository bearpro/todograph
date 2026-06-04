module Control.TodoGraphItem exposing (..)

import Browser.Dom as Dom
import Control.FluentIcon as FluentIcon
import Domain.Project as Project
import Html exposing (Attribute, Html, button, div, input, li, span, text, ul)
import Html.Attributes exposing (attribute, autofocus, checked, class, id, style, title, type_, value)
import Html.Events exposing (on, onBlur, onCheck, onClick, onInput)
import Json.Decode as Decode
import Platform.Cmd as Cmd
import Task
import Time
import UUID exposing (UUID)


type TextEditState
    = NotEditing
    | EditingText String
    | EditingDescription String


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
    | StartDescriptionEdit String
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
            ( { model | textEditState = EditingText model.node.text }
            , Dom.focus (textInputId model.node.id)
                |> Task.attempt TextInputFocused
            )

        StartDescriptionEdit description ->
            ( { model | textEditState = EditingDescription description }
            , Dom.focus (descriptionInputId model.node.id)
                |> Task.attempt TextInputFocused
            )

        TextInputFocused _ ->
            ( model, Cmd.none )

        StopTextEdit ->
            case model.textEditState of
                EditingDescription draft ->
                    ( { model
                        | node = commitNodeDescription draft model.node
                        , textEditState = NotEditing
                      }
                    , Cmd.none
                    )

                _ ->
                    ( { model | textEditState = NotEditing }
                    , Cmd.none
                    )

        DraftChanged newDraftText ->
            case model.textEditState of
                EditingText _ ->
                    ( { model
                        | node = updateNodeText newDraftText model.node
                        , textEditState = EditingText newDraftText
                      }
                    , Cmd.none
                    )

                EditingDescription _ ->
                    ( { model
                        | textEditState = EditingDescription newDraftText
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


commitNodeDescription : String -> Project.Node -> Project.Node
commitNodeDescription description node =
    if String.trim description == "" then
        { node | description = Nothing }

    else
        { node | description = Just description }


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
                [ class (nodeTextClass node)
                , onClick StartTextEdit
                , style "cursor" "text"
                , style "min-width" "0"
                ]
                [ text node.text ]

        EditingText draft ->
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

        EditingDescription _ ->
            span
                [ class (nodeTextClass node)
                , onClick StartTextEdit
                , style "cursor" "text"
                , style "min-width" "0"
                ]
                [ text node.text ]


nodeTextClass : Project.Node -> String
nodeTextClass node =
    if node.status then
        "flex-grow-1 text-decoration-line-through"

    else
        "flex-grow-1"


textInputId : UUID -> String
textInputId nodeId =
    "todo-graph-node-text-" ++ UUID.toString nodeId


descriptionInputId : UUID -> String
descriptionInputId nodeId =
    "todo-graph-node-description-" ++ UUID.toString nodeId


viewDescription : Project.Node -> TextEditState -> String -> Html Msg
viewDescription node editState description =
    case editState of
        EditingDescription draft ->
            input
                [ value draft
                , id (descriptionInputId node.id)
                , onInput DraftChanged
                , onBlur StopTextEdit
                , onEnter StopTextEdit
                , autofocus True
                , class "form-control form-control-sm flex-grow-1"
                ]
                []

        _ ->
            span
                [ class "flex-grow-1"
                , onClick (StartDescriptionEdit description)
                , style "cursor" "text"
                , style "min-width" "0"
                ]
                [ text description ]


viewTimer : ViewOptions -> Maybe Project.Timer -> List (Html Msg)
viewTimer options maybeTimer =
    case maybeTimer of
        Just timer ->
            [ ul [ class "list-group list-group-flush" ]
                [ li [ class "list-group-item d-flex align-items-center justify-content-between gap-2 px-3 py-1" ]
                    [ span [ class "text-muted small" ] [ text (formatSeconds (timerSeconds options.now timer)) ]
                    , button
                        ([ onClick (timerButtonMsg timer)
                         , type_ "button"
                         , class (timerButtonClass timer ++ " btn-icon")
                         , title (timerButtonLabel timer)
                         , attribute "aria-label" (timerButtonLabel timer)
                         ]
                            ++ hiddenStyles options.hideButtons
                        )
                        [ FluentIcon.view (timerButtonIcon timer) ]
                    ]
                ]
            ]

        Nothing ->
            []


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


timerButtonIcon : Project.Timer -> FluentIcon.Icon
timerButtonIcon timer =
    case timer of
        Project.Started _ ->
            FluentIcon.Stop

        Project.Stopped seconds ->
            if seconds > 0 then
                FluentIcon.ArrowClockwise

            else
                FluentIcon.Play


timerButtonClass : Project.Timer -> String
timerButtonClass timer =
    case timer of
        Project.Started _ ->
            "btn btn-sm btn-success"

        Project.Stopped seconds ->
            if seconds > 0 then
                "btn btn-sm btn-warning"

            else
                "btn btn-sm btn-outline-secondary"


hiddenStyles : Bool -> List (Attribute Msg)
hiddenStyles hidden =
    if hidden then
        [ style "visibility" "hidden"
        , style "pointer-events" "none"
        ]

    else
        []


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


viewContent : ViewOptions -> Model -> List (Html Msg)
viewContent options model =
    case model.node.description of
        Just description ->
            [ div
                [ class "card-header d-flex align-items-start gap-2 px-3 py-2"
                , style "min-width" "0"
                ]
                [ viewCheckbox model.node
                , viewText model.node model.textEditState
                ]
            , div
                [ class "card-body d-flex align-items-start gap-2 px-3 py-2"
                , style "min-width" "0"
                ]
                [ viewDescription model.node model.textEditState description ]
            ]
                ++ viewTimer options model.node.timer

        Nothing ->
            div
                [ class "card-body d-flex align-items-start gap-2 px-3 py-2"
                , style "min-width" "0"
                ]
                [ viewCheckbox model.node
                , viewText model.node model.textEditState
                ]
                :: viewTimer options model.node.timer


viewCheckbox : Project.Node -> Html Msg
viewCheckbox node =
    input
        [ type_ "checkbox"
        , checked node.status
        , onCheck CheckboxToggle
        , class "form-check-input todo-graph-node-checkbox"
        , style "width" "20px"
        , style "height" "20px"
        , style "flex" "0 0 auto"
        ]
        []


view : ViewOptions -> Model -> Html Msg
view options model =
    div
        [ class "card d-flex flex-column"
        , style "box-sizing" "border-box"
        ]
        (viewContent options model)
