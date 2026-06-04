module Page.TodoGraph exposing (..)

import Browser exposing (Document)
import Browser.Dom as Dom
import Browser.Events as BrowserEvents
import Control.FluentIcon as FluentIcon
import Control.TodoGraphItem as TodoGraphItem
import Domain.Project as Project
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (attribute, class, disabled, id, style, title, type_)
import Html.Events exposing (custom, on, onClick, onMouseEnter, onMouseLeave)
import Json.Decode as Decode
import Page.TodoGraph.Layout as Layout
import Platform.Cmd as Cmd
import Random
import Task
import Time
import UUID exposing (UUID)


type alias Model =
    { project : Project.Project
    , nodeUiStates : List NodeUiState
    , joinDrag : Maybe JoinDrag
    , now : Maybe Time.Posix
    , openAddMenu : Maybe UUID
    , nodeHeights : List NodeHeight
    }


type alias NodeUiState =
    { nodeId : UUID
    , textEditState : TodoGraphItem.TextEditState
    }


type alias NodeHeight =
    { nodeId : UUID
    , height : Int
    }


type alias JoinDrag =
    { sourceChainId : UUID
    , sourceNodeId : UUID
    , mode : JoinMode
    , startMouse : MousePoint
    , mouse : MousePoint
    , activePointerId : Maybe Int
    , backgroundPointerStart : Maybe JoinPointer
    , graphOrigin : Maybe MousePoint
    , sourceButtonCenter : Maybe MousePoint
    , hoveredNodeId : Maybe UUID
    }


type JoinMode
    = JoinPressing
    | JoinDragging
    | JoinLatched


type alias MousePoint =
    { x : Float
    , y : Float
    }


type alias JoinPointer =
    { id : Int
    , point : MousePoint
    }


type Msg
    = GraphItemMsg UUID TodoGraphItem.Msg
    | ToggleAddMenu UUID
    | CreateTextNodeAfter UUID
    | TextNodeAfterGenerated UUID UUID
    | AddTimer UUID
    | AddDescription UUID
    | CreateFork UUID UUID
    | ForkGenerated UUID UUID UUID UUID
    | StartJoinPress UUID UUID JoinPointer
    | Unjoin UUID
    | GraphElementMeasured (Result Dom.Error Dom.Element)
    | JoinButtonMeasured (Result Dom.Error Dom.Element)
    | NodeCardMeasured UUID (Result Dom.Error Dom.Element)
    | MoveJoinPointer JoinPointer
    | ReleaseJoinPointer JoinPointer
    | BackgroundJoinPointerDown JoinPointer
    | JoinNodePointerDown JoinPointer
    | JoinNodePointerUp UUID JoinPointer
    | HoverJoinTarget UUID
    | LeaveJoinTarget UUID
    | DeleteNode UUID
    | Tick Time.Posix


cardWidth : Int
cardWidth =
    280


nodeHeight : Int
nodeHeight =
    132


columnStep : Int
columnStep =
    380


nodeGridWidth : Int
nodeGridWidth =
    cardWidth


columnGap : Int
columnGap =
    columnStep - cardWidth


rowGap : Int
rowGap =
    createButtonOverhang * 3 // 2


createButtonSize : Int
createButtonSize =
    28


createButtonGap : Int
createButtonGap =
    4


createButtonSideGap : Int
createButtonSideGap =
    6


createButtonOverhang : Int
createButtonOverhang =
    createButtonSize + createButtonGap


createButtonSideOverhang : Int
createButtonSideOverhang =
    createButtonSize + createButtonSideGap


edgeColor : String
edgeColor =
    "#61666d"


joinDragThreshold : Float
joinDragThreshold =
    6


mouseFallbackPointerId : Int
mouseFallbackPointerId =
    -1


graphRootId : String
graphRootId =
    "todo-graph-root"


init : Model -> ( Model, Cmd Msg )
init model =
    ( model, measureProjectNodes model.project )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GraphItemMsg nodeId graphItemMsg ->
            updateGraphItem nodeId graphItemMsg model

        ToggleAddMenu nodeId ->
            ( { model
                | openAddMenu =
                    if model.openAddMenu == Just nodeId then
                        Nothing

                    else
                        Just nodeId
              }
            , Cmd.none
            )

        CreateTextNodeAfter nodeId ->
            ( model
            , Random.generate (TextNodeAfterGenerated nodeId) UUID.generator
            )

        TextNodeAfterGenerated afterNodeId nodeId ->
            let
                nextProject =
                    model.project
                        |> Project.insertNodeAfter afterNodeId (Project.textNode nodeId "item")
            in
            ( { model
                | project = nextProject
                , openAddMenu = Nothing
              }
            , measureProjectNodes nextProject
            )

        AddTimer nodeId ->
            let
                nextProject =
                    model.project
                        |> Project.addTimerToNode nodeId
            in
            ( { model
                | project = nextProject
                , openAddMenu = Nothing
              }
            , measureProjectNodes nextProject
            )

        AddDescription nodeId ->
            let
                nextProject =
                    model.project
                        |> Project.addDescriptionToNode nodeId
            in
            ( { model
                | project = nextProject
                , nodeUiStates =
                    upsertNodeEditState nodeId (TodoGraphItem.EditingDescription "") model.nodeUiStates
                , openAddMenu = Nothing
              }
            , Cmd.batch
                [ measureProjectNodes nextProject
                , Dom.focus (TodoGraphItem.descriptionInputId nodeId)
                    |> Task.attempt (GraphItemMsg nodeId << TodoGraphItem.TextInputFocused)
                ]
            )

        CreateFork sourceChainId sourceNodeId ->
            ( model
            , Random.generate
                (\( chainId, nodeId ) ->
                    ForkGenerated sourceChainId sourceNodeId chainId nodeId
                )
                (Random.map2 Tuple.pair UUID.generator UUID.generator)
            )

        ForkGenerated sourceChainId sourceNodeId chainId nodeId ->
            let
                nextProject =
                    model.project
                        |> Project.forkChain
                            sourceChainId
                            sourceNodeId
                            chainId
                            (Project.textNode nodeId "item")
            in
            ( { model
                | project = nextProject
                , openAddMenu = Nothing
              }
            , measureProjectNodes nextProject
            )

        StartJoinPress sourceChainId sourceNodeId pointer ->
            case model.joinDrag of
                Just joinDrag ->
                    if joinDrag.mode == JoinLatched && joinDrag.sourceNodeId == sourceNodeId then
                        ( { model | joinDrag = Nothing, openAddMenu = Nothing }, Cmd.none )

                    else
                        startJoinPress sourceChainId sourceNodeId pointer model

                Nothing ->
                    startJoinPress sourceChainId sourceNodeId pointer model

        Unjoin chainId ->
            let
                nextProject =
                    Project.unjoinChain chainId model.project
            in
            ( { model
                | project = nextProject
                , openAddMenu = Nothing
              }
            , measureProjectNodes nextProject
            )

        GraphElementMeasured result ->
            case ( result, model.joinDrag ) of
                ( Ok element, Just joinDrag ) ->
                    ( { model
                        | joinDrag =
                            Just
                                { joinDrag
                                    | graphOrigin =
                                        Just
                                            { x = element.element.x
                                            , y = element.element.y
                                            }
                                }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        JoinButtonMeasured result ->
            case ( result, model.joinDrag ) of
                ( Ok element, Just joinDrag ) ->
                    ( { model
                        | joinDrag =
                            Just
                                { joinDrag
                                    | sourceButtonCenter =
                                        Just
                                            { x = element.element.x + (element.element.width / 2)
                                            , y = element.element.y + (element.element.height / 2)
                                            }
                                }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        NodeCardMeasured nodeId result ->
            case result of
                Ok element ->
                    ( { model
                        | nodeHeights =
                            upsertNodeHeight
                                { nodeId = nodeId
                                , height = max 1 (ceiling element.element.height)
                                }
                                model.nodeHeights
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        MoveJoinPointer pointer ->
            ( { model
                | joinDrag =
                    model.joinDrag
                        |> Maybe.map (moveJoinPointer pointer)
              }
            , Cmd.none
            )

        ReleaseJoinPointer pointer ->
            case model.joinDrag of
                Just joinDrag ->
                    releaseJoinPointer pointer joinDrag model

                Nothing ->
                    ( model, Cmd.none )

        BackgroundJoinPointerDown pointer ->
            ( { model
                | joinDrag =
                    model.joinDrag
                        |> Maybe.map
                            (\joinDrag ->
                                if joinDrag.mode == JoinLatched then
                                    { joinDrag
                                        | mouse = pointer.point
                                        , backgroundPointerStart = Just pointer
                                    }

                                else
                                    joinDrag
                            )
              }
            , Cmd.none
            )

        JoinNodePointerDown pointer ->
            ( { model
                | joinDrag =
                    model.joinDrag
                        |> Maybe.map
                            (\joinDrag ->
                                if joinDrag.mode == JoinLatched then
                                    { joinDrag
                                        | mouse = pointer.point
                                        , backgroundPointerStart = Nothing
                                    }

                                else
                                    joinDrag
                            )
              }
            , Cmd.none
            )

        JoinNodePointerUp nodeId pointer ->
            case model.joinDrag of
                Just joinDrag ->
                    releaseJoinNodePointer nodeId pointer joinDrag model

                Nothing ->
                    ( model, Cmd.none )

        HoverJoinTarget nodeId ->
            ( { model
                | joinDrag =
                    model.joinDrag
                        |> Maybe.map
                            (\joinDrag ->
                                if Project.canJoinChainToNode joinDrag.sourceChainId nodeId model.project then
                                    { joinDrag | hoveredNodeId = Just nodeId }

                                else
                                    joinDrag
                            )
              }
            , Cmd.none
            )

        LeaveJoinTarget nodeId ->
            ( { model
                | joinDrag =
                    model.joinDrag
                        |> Maybe.map
                            (\joinDrag ->
                                if joinDrag.hoveredNodeId == Just nodeId then
                                    { joinDrag | hoveredNodeId = Nothing }

                                else
                                    joinDrag
                            )
              }
            , Cmd.none
            )

        DeleteNode nodeId ->
            let
                nextProject =
                    Project.deleteNode nodeId model.project
            in
            ( { model
                | project = nextProject
                , nodeUiStates =
                    model.nodeUiStates
                        |> List.filter (.nodeId >> (/=) nodeId)
                , nodeHeights =
                    model.nodeHeights
                        |> List.filter (.nodeId >> (/=) nodeId)
                , openAddMenu =
                    if model.openAddMenu == Just nodeId then
                        Nothing

                    else
                        model.openAddMenu
              }
            , measureProjectNodes nextProject
            )

        Tick now ->
            ( { model | now = Just now }
            , Cmd.none
            )


startJoinPress : UUID -> UUID -> JoinPointer -> Model -> ( Model, Cmd Msg )
startJoinPress sourceChainId sourceNodeId pointer model =
    ( { model
        | joinDrag =
            Just
                { sourceChainId = sourceChainId
                , sourceNodeId = sourceNodeId
                , mode = JoinPressing
                , startMouse = pointer.point
                , mouse = pointer.point
                , activePointerId = Just pointer.id
                , backgroundPointerStart = Nothing
                , graphOrigin = Nothing
                , sourceButtonCenter = Nothing
                , hoveredNodeId = Nothing
                }
        , openAddMenu = Nothing
      }
    , Cmd.batch
        [ Dom.getElement graphRootId
            |> Task.attempt GraphElementMeasured
        , Dom.getElement (joinButtonId sourceNodeId)
            |> Task.attempt JoinButtonMeasured
        ]
    )


moveJoinPointer : JoinPointer -> JoinDrag -> JoinDrag
moveJoinPointer pointer joinDrag =
    case joinDrag.mode of
        JoinPressing ->
            if isActiveJoinPointer pointer joinDrag then
                { joinDrag
                    | mouse = pointer.point
                    , mode =
                        if movedBeyondJoinThreshold joinDrag.startMouse pointer.point then
                            JoinDragging

                        else
                            JoinPressing
                }

            else
                joinDrag

        JoinDragging ->
            if isActiveJoinPointer pointer joinDrag then
                { joinDrag | mouse = pointer.point }

            else
                joinDrag

        JoinLatched ->
            { joinDrag | mouse = pointer.point }


releaseJoinPointer : JoinPointer -> JoinDrag -> Model -> ( Model, Cmd Msg )
releaseJoinPointer pointer joinDrag model =
    case joinDrag.mode of
        JoinPressing ->
            if isActiveJoinPointer pointer joinDrag then
                ( { model
                    | joinDrag =
                        Just
                            { joinDrag
                                | mode = JoinLatched
                                , mouse = pointer.point
                                , activePointerId = Nothing
                            }
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        JoinDragging ->
            if isActiveJoinPointer pointer joinDrag then
                finishJoin joinDrag.hoveredNodeId model

            else
                ( model, Cmd.none )

        JoinLatched ->
            case joinDrag.backgroundPointerStart of
                Just startPointer ->
                    if startPointer.id == pointer.id || pointer.id == mouseFallbackPointerId then
                        if movedBeyondJoinThreshold startPointer.point pointer.point then
                            ( { model
                                | joinDrag =
                                    Just
                                        { joinDrag
                                            | mouse = pointer.point
                                            , backgroundPointerStart = Nothing
                                        }
                              }
                            , Cmd.none
                            )

                        else
                            cancelJoin model

                    else
                        ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )


releaseJoinNodePointer : UUID -> JoinPointer -> JoinDrag -> Model -> ( Model, Cmd Msg )
releaseJoinNodePointer nodeId pointer joinDrag model =
    case joinDrag.mode of
        JoinPressing ->
            if joinDrag.sourceNodeId == nodeId && isActiveJoinPointer pointer joinDrag then
                ( { model
                    | joinDrag =
                        Just
                            { joinDrag
                                | mode = JoinLatched
                                , mouse = pointer.point
                                , activePointerId = Nothing
                            }
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        JoinDragging ->
            if Project.canJoinChainToNode joinDrag.sourceChainId nodeId model.project then
                finishJoin (Just nodeId) model

            else
                cancelJoin model

        JoinLatched ->
            if Project.canJoinChainToNode joinDrag.sourceChainId nodeId model.project then
                finishJoin (Just nodeId) model

            else
                ( { model
                    | joinDrag =
                        Just
                            { joinDrag
                                | mouse = pointer.point
                                , backgroundPointerStart = Nothing
                            }
                  }
                , Cmd.none
                )


finishJoin : Maybe UUID -> Model -> ( Model, Cmd Msg )
finishJoin maybeTargetNodeId model =
    case model.joinDrag of
        Just joinDrag ->
            case maybeTargetNodeId of
                Just targetNodeId ->
                    if Project.canJoinChainToNode joinDrag.sourceChainId targetNodeId model.project then
                        let
                            nextProject =
                                Project.joinChainToNode joinDrag.sourceChainId targetNodeId model.project
                        in
                        ( { model
                            | project = nextProject
                            , joinDrag = Nothing
                            , openAddMenu = Nothing
                          }
                        , measureProjectNodes nextProject
                        )

                    else
                        cancelJoin model

                Nothing ->
                    cancelJoin model

        Nothing ->
            ( model, Cmd.none )


cancelJoin : Model -> ( Model, Cmd Msg )
cancelJoin model =
    ( { model | joinDrag = Nothing, openAddMenu = Nothing }, Cmd.none )


isActiveJoinPointer : JoinPointer -> JoinDrag -> Bool
isActiveJoinPointer pointer joinDrag =
    if pointer.id == mouseFallbackPointerId then
        True

    else
        case joinDrag.activePointerId of
            Just activePointerId ->
                activePointerId == pointer.id

            Nothing ->
                True


movedBeyondJoinThreshold : MousePoint -> MousePoint -> Bool
movedBeyondJoinThreshold start current =
    let
        dx =
            current.x - start.x

        dy =
            current.y - start.y
    in
    ((dx * dx) + (dy * dy)) >= (joinDragThreshold * joinDragThreshold)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ if hasRunningTimer model.project then
            Time.every 1000 Tick

          else
            Sub.none
        , case model.joinDrag of
            Just _ ->
                Sub.batch
                    [ BrowserEvents.onMouseMove (Decode.map MoveJoinPointer mouseJoinPointerDecoder)
                    , BrowserEvents.onMouseUp (Decode.map ReleaseJoinPointer mouseJoinPointerDecoder)
                    ]

            Nothing ->
                Sub.none
        ]


hasRunningTimer : Project.Project -> Bool
hasRunningTimer project =
    project.chains
        |> List.concatMap .nodes
        |> List.any
            (\node ->
                case node.timer of
                    Just (Project.Started _) ->
                        True

                    _ ->
                        False
            )


updateGraphItem : UUID -> TodoGraphItem.Msg -> Model -> ( Model, Cmd Msg )
updateGraphItem nodeId graphItemMsg model =
    case findNode nodeId model.project of
        Just node ->
            let
                controlModel =
                    controlModelFor node model

                ( updatedControlModel, command ) =
                    TodoGraphItem.update graphItemMsg controlModel

                nextNow =
                    case graphItemMsg of
                        TodoGraphItem.TimerStarted now ->
                            Just now

                        TodoGraphItem.TimerStopped now ->
                            Just now

                        _ ->
                            model.now

                nextProject =
                    model.project
                        |> Project.updateNode nodeId updatedControlModel.node
            in
            ( { model
                | project = nextProject
                , nodeUiStates =
                    upsertNodeUiState updatedControlModel model.nodeUiStates
                , now = nextNow
              }
            , Cmd.batch
                [ Cmd.map (GraphItemMsg nodeId) command
                , measureProjectNodes nextProject
                ]
            )

        Nothing ->
            ( model, Cmd.none )


controlModelFor : Project.Node -> Model -> TodoGraphItem.Model
controlModelFor node model =
    let
        baseModel =
            TodoGraphItem.fromNode node
    in
    model.nodeUiStates
        |> List.filter (.nodeId >> (==) node.id)
        |> List.head
        |> Maybe.map
            (\uiState ->
                { baseModel | textEditState = uiState.textEditState }
            )
        |> Maybe.withDefault baseModel


upsertNodeUiState : TodoGraphItem.Model -> List NodeUiState -> List NodeUiState
upsertNodeUiState controlModel nodeUiStates =
    let
        nextUiState =
            { nodeId = controlModel.node.id
            , textEditState = controlModel.textEditState
            }

        replaceExisting uiState =
            if uiState.nodeId == controlModel.node.id then
                nextUiState

            else
                uiState

        exists =
            nodeUiStates
                |> List.any (.nodeId >> (==) controlModel.node.id)
    in
    if exists then
        nodeUiStates |> List.map replaceExisting

    else
        nextUiState :: nodeUiStates


upsertNodeEditState : UUID -> TodoGraphItem.TextEditState -> List NodeUiState -> List NodeUiState
upsertNodeEditState nodeId editState nodeUiStates =
    let
        nextUiState =
            { nodeId = nodeId
            , textEditState = editState
            }

        replaceExisting uiState =
            if uiState.nodeId == nodeId then
                nextUiState

            else
                uiState

        exists =
            nodeUiStates
                |> List.any (.nodeId >> (==) nodeId)
    in
    if exists then
        nodeUiStates |> List.map replaceExisting

    else
        nextUiState :: nodeUiStates


measureProjectNodes : Project.Project -> Cmd Msg
measureProjectNodes project =
    project.chains
        |> List.concatMap .nodes
        |> List.map
            (\node ->
                Dom.getElement (nodeCardId node.id)
                    |> Task.attempt (NodeCardMeasured node.id)
            )
        |> Cmd.batch


nodeCardId : UUID -> String
nodeCardId nodeId =
    "todo-graph-node-card-" ++ UUID.toString nodeId


joinButtonId : UUID -> String
joinButtonId nodeId =
    "todo-graph-node-join-" ++ UUID.toString nodeId


upsertNodeHeight : NodeHeight -> List NodeHeight -> List NodeHeight
upsertNodeHeight nextHeight nodeHeights =
    let
        replaceExisting cachedHeight =
            if cachedHeight.nodeId == nextHeight.nodeId then
                nextHeight

            else
                cachedHeight

        exists =
            nodeHeights
                |> List.any (.nodeId >> (==) nextHeight.nodeId)
    in
    if exists then
        nodeHeights |> List.map replaceExisting

    else
        nextHeight :: nodeHeights


findNode : UUID -> Project.Project -> Maybe Project.Node
findNode nodeId project =
    project.chains
        |> List.concatMap .nodes
        |> List.filter (.id >> (==) nodeId)
        |> List.head


view : Model -> Document Msg
view model =
    let
        renderChains =
            model.project.chains
                |> Layout.layoutChains
                |> List.map
                    (\layout ->
                        let
                            chain =
                                layout.chain
                        in
                        { chain | order = layout.physicalColumn }
                    )

        maxRow =
            maxGraphRow renderChains

        hideButtons =
            model.joinDrag /= Nothing
    in
    { title = Maybe.withDefault ("Project " ++ shortUuid model.project.id) model.project.name
    , body =
        [ div
            [ style "height" "calc(100vh - 57px)"
            , style "overflow" "auto"
            , style "box-sizing" "border-box"
            , style "background" "#fff"
            , style "padding" "1rem"
            ]
            [ div
                [ style "min-height" "100%"
                , style "display" "flex"
                , style "align-items" "flex-end"
                , style "justify-content" "flex-start"
                ]
                [ div
                    ([ id graphRootId
                     , style "position" "relative"
                     , style "display" "grid"
                     , style "grid-template-columns" ("repeat(" ++ String.fromInt (columnCount renderChains) ++ ", " ++ px cardWidth ++ ")")
                     , style "grid-template-rows" (gridTemplateRows model renderChains)
                     , style "column-gap" (px columnGap)
                     , style "row-gap" (px rowGap)
                     , style "align-content" "end"
                     , style "justify-content" "start"
                     , style "width" (px (graphWidth renderChains))
                     , style "height" (px (graphHeight model renderChains))
                     , style "min-width" (px (graphWidth renderChains))
                     , style "min-height" (px (graphHeight model renderChains))
                     ]
                        ++ joinGraphPointerAttributes model.joinDrag
                    )
                    (viewEdges model renderChains
                        ++ viewDragEdge model renderChains model.joinDrag
                        ++ viewNodes hideButtons maxRow model renderChains
                    )
                ]
            ]
        ]
    }


viewNodes : Bool -> Int -> Model -> List Project.Chain -> List (Html Msg)
viewNodes hideButtons maxRow model columns =
    columns
        |> List.concatMap
            (\column ->
                column.nodes
                    |> List.indexedMap (viewNode hideButtons maxRow model columns column)
            )


viewNode : Bool -> Int -> Model -> List Project.Chain -> Project.Chain -> Int -> Project.Node -> Html Msg
viewNode hideButtons maxRow model columns column index node =
    let
        currentRow =
            nodeRowValue column index

        cardHeight =
            nodeCardHeight model node

        itemContent =
            TodoGraphItem.viewContent
                { hideButtons = hideButtons
                , now = model.now
                }
                (controlModelFor node model)
                |> List.map (Html.map (GraphItemMsg node.id))
    in
    div
        ([ style "position" "relative"
         , style "grid-column" (String.fromInt (column.order + 1))
         , style "grid-row" (String.fromInt (nodeGridRow maxRow column index))
         , style "width" (px nodeGridWidth)
         , style "height" (px (rowHeight model columns currentRow))
         , style "align-self" "stretch"
         , style "justify-self" "start"
         , style "z-index"
            (if model.openAddMenu == Just node.id then
                "8"

             else
                "3"
            )
         , onMouseEnter (HoverJoinTarget node.id)
         , onMouseLeave (LeaveJoinTarget node.id)
         ]
            ++ joinNodePointerAttributes model.joinDrag node.id
        )
        [ viewNewItemButton model.joinDrag model.project node cardHeight
        , viewNewChainButton model.joinDrag model.project column node cardHeight
        , div
            ([ class "card d-flex flex-column"
             , id (nodeCardId node.id)
             , style "position" "absolute"
             , style "left" "0"
             , style "bottom" "0"
             , style "width" (px cardWidth)
             , style "box-sizing" "border-box"
             ]
                ++ joinTargetStyles model.project model.joinDrag column node
            )
            (itemContent
                ++ viewCardFooter model.joinDrag model.project model.openAddMenu column index node
            )
        ]


viewNewItemButton : Maybe JoinDrag -> Project.Project -> Project.Node -> Int -> Html Msg
viewNewItemButton maybeJoinDrag project node cardHeight =
    let
        isJoining =
            maybeJoinDrag /= Nothing
    in
    button
        ([ type_ "button"
         , class "btn btn-outline-primary todo-graph-create-button"
         , title "New item"
         , attribute "aria-label" "New item"
         , onClick (CreateTextNodeAfter node.id)
         , disabled (isJoining || not (Project.canCreateAfterNode node.id project))
         , style "position" "absolute"
         , style "left" "0"
         , style "bottom" (px (cardHeight + createButtonGap))
         , style "z-index" "9"
         ]
            ++ hiddenStyles isJoining
        )
        [ FluentIcon.view FluentIcon.AddSquare ]


viewNewChainButton : Maybe JoinDrag -> Project.Project -> Project.Chain -> Project.Node -> Int -> Html Msg
viewNewChainButton maybeJoinDrag project column node cardHeight =
    let
        isJoining =
            maybeJoinDrag /= Nothing
    in
    button
        ([ type_ "button"
         , class "btn btn-outline-primary todo-graph-create-button"
         , title "New chain"
         , attribute "aria-label" "New chain"
         , onClick (CreateFork column.id node.id)
         , disabled (isJoining || not (Project.canCreateAfterNode node.id project))
         , style "position" "absolute"
         , style "left" (px (cardWidth + createButtonSideGap))
         , style "bottom" (px (max 0 (cardHeight - createButtonSize)))
         , style "z-index" "9"
         ]
            ++ hiddenStyles isJoining
        )
        [ FluentIcon.view FluentIcon.AddSquare ]


viewCardFooter : Maybe JoinDrag -> Project.Project -> Maybe UUID -> Project.Chain -> Int -> Project.Node -> List (Html Msg)
viewCardFooter maybeJoinDrag project openAddMenu column index node =
    [ div
        [ class "card-footer p-2" ]
        [ viewActionGroup maybeJoinDrag project openAddMenu column index node ]
    ]


hiddenStyles : Bool -> List (Html.Attribute Msg)
hiddenStyles hidden =
    if hidden then
        [ style "visibility" "hidden"
        , style "pointer-events" "none"
        ]

    else
        []


joinGraphPointerAttributes : Maybe JoinDrag -> List (Html.Attribute Msg)
joinGraphPointerAttributes maybeJoinDrag =
    case maybeJoinDrag of
        Just _ ->
            [ on "pointermove" (Decode.map MoveJoinPointer joinPointerDecoder)
            , on "pointerup" (Decode.map ReleaseJoinPointer joinPointerDecoder)
            , on "pointerdown" (Decode.map BackgroundJoinPointerDown joinPointerDecoder)
            ]

        Nothing ->
            []


joinNodePointerAttributes : Maybe JoinDrag -> UUID -> List (Html.Attribute Msg)
joinNodePointerAttributes maybeJoinDrag nodeId =
    case maybeJoinDrag of
        Just _ ->
            [ onPointerStopAndPrevent "pointerdown" JoinNodePointerDown
            , onPointerStopAndPrevent "pointerup" (JoinNodePointerUp nodeId)
            ]

        Nothing ->
            []


onPointerStopAndPrevent : String -> (JoinPointer -> Msg) -> Html.Attribute Msg
onPointerStopAndPrevent eventName toMsg =
    custom eventName
        (Decode.map
            (\pointer ->
                { message = toMsg pointer
                , stopPropagation = True
                , preventDefault = True
                }
            )
            joinPointerDecoder
        )


viewActionGroup : Maybe JoinDrag -> Project.Project -> Maybe UUID -> Project.Chain -> Int -> Project.Node -> Html Msg
viewActionGroup maybeJoinDrag project openAddMenu column index node =
    div
        [ class "btn-group btn-group-sm"
        , attribute "role" "group"
        , style "position" "relative"
        ]
        (viewAddDropdown maybeJoinDrag openAddMenu column index node
            ++ viewJoinButton maybeJoinDrag column index node
            ++ viewDeleteButton maybeJoinDrag project node
        )


viewAddDropdown : Maybe JoinDrag -> Maybe UUID -> Project.Chain -> Int -> Project.Node -> List (Html Msg)
viewAddDropdown maybeJoinDrag openAddMenu column index node =
    let
        isJoining =
            maybeJoinDrag /= Nothing

        isOpen =
            openAddMenu == Just node.id

        opensUp =
            nodeRowValue column index == 0

        hasTimer =
            node.timer /= Nothing

        hasDescription =
            node.description /= Nothing
    in
    button
        ([ type_ "button"
         , class "btn btn-outline-primary btn-icon dropdown-toggle"
         , title "Add"
         , attribute "aria-label" "Add"
         , onClick (ToggleAddMenu node.id)
         , disabled isJoining
         ]
            ++ hiddenStyles isJoining
        )
        [ FluentIcon.view FluentIcon.AddCircle ]
        :: (if not isJoining && isOpen then
                [ div
                    ([ class "dropdown-menu show"
                     , style "display" "block"
                     , style "position" "absolute"
                     , style "left" "0"
                     , style "z-index" "20"
                     ]
                        ++ dropdownVerticalStyles opensUp
                    )
                    [ button
                        [ type_ "button"
                        , class "dropdown-item"
                        , onClick (AddTimer node.id)
                        , disabled hasTimer
                        ]
                        [ text "Timer" ]
                    , button
                        [ type_ "button"
                        , class "dropdown-item"
                        , onClick (AddDescription node.id)
                        , disabled hasDescription
                        ]
                        [ text "Description" ]
                    ]
                ]

            else
                []
           )


dropdownVerticalStyles : Bool -> List (Html.Attribute Msg)
dropdownVerticalStyles opensUp =
    if opensUp then
        [ style "bottom" "calc(100% + 4px)" ]

    else
        [ style "top" "calc(100% + 4px)" ]


viewJoinButton : Maybe JoinDrag -> Project.Chain -> Int -> Project.Node -> List (Html Msg)
viewJoinButton maybeJoinDrag column index node =
    let
        isJoining =
            maybeJoinDrag /= Nothing

        isSource =
            maybeJoinDrag
                |> Maybe.map (\joinDrag -> joinDrag.sourceNodeId == node.id)
                |> Maybe.withDefault False

        isJoinSource =
            Project.isJoinSource node.id column
    in
    if isJoinSource then
        [ button
            ([ type_ "button"
             , id (joinButtonId node.id)
             , class "btn btn-outline-primary btn-icon"
             , title "Unjoin"
             , attribute "aria-label" "Unjoin"
             , onClick (Unjoin column.id)
             , disabled isJoining
             ]
                ++ hiddenStyles isJoining
            )
            [ FluentIcon.view FluentIcon.LinkDismiss ]
        ]

    else
        [ button
            ([ onPointerStopAndPrevent "pointerdown" (StartJoinPress column.id node.id)
             , type_ "button"
             , id (joinButtonId node.id)
             , class "btn btn-outline-primary btn-icon todo-graph-join-button"
             , title "Join"
             , attribute "aria-label" "Join"
             , disabled ((isJoining && not isSource) || not (canStartJoinDrag column index))
             ]
                ++ hiddenStyles (isJoining && not isSource)
            )
            [ FluentIcon.view FluentIcon.Link ]
        ]


canStartJoinDrag : Project.Chain -> Int -> Bool
canStartJoinDrag column index =
    (column.forkedFrom /= Nothing)
        && (column.joinedInto == Nothing)
        && (index == List.length column.nodes - 1)


viewDeleteButton : Maybe JoinDrag -> Project.Project -> Project.Node -> List (Html Msg)
viewDeleteButton maybeJoinDrag project node =
    let
        isJoining =
            maybeJoinDrag /= Nothing
    in
    [ button
        ([ type_ "button"
         , onClick (DeleteNode node.id)
         , class "btn btn-outline-danger btn-icon"
         , title "Delete node"
         , attribute "aria-label" "Delete node"
         , disabled (isJoining || not (Project.canDeleteNode node.id project))
         ]
            ++ hiddenStyles isJoining
        )
        [ FluentIcon.view FluentIcon.Delete ]
    ]


joinTargetStyles : Project.Project -> Maybe JoinDrag -> Project.Chain -> Project.Node -> List (Html.Attribute Msg)
joinTargetStyles project maybeJoinDrag _ node =
    case maybeJoinDrag of
        Just joinDrag ->
            if node.id == joinDrag.sourceNodeId then
                []

            else if Project.canJoinChainToNode joinDrag.sourceChainId node.id project then
                if joinDrag.hoveredNodeId == Just node.id then
                    [ style "outline" "2px solid #0d6efd"
                    , style "outline-offset" "3px"
                    , style "box-shadow" "0 0 0 4px rgba(13, 110, 253, 0.18)"
                    ]

                else
                    []

            else
                [ style "opacity" "0.38"
                , style "filter" "grayscale(1)"
                ]

        Nothing ->
            []


viewEdges : Model -> List Project.Chain -> List (Html Msg)
viewEdges model columns =
    viewVerticalEdges model columns
        ++ viewForkEdges model columns
        ++ viewJoinEdges model columns


viewDragEdge : Model -> List Project.Chain -> Maybe JoinDrag -> List (Html Msg)
viewDragEdge model columns maybeJoinDrag =
    case maybeJoinDrag of
        Just joinDrag ->
            case Project.findChain joinDrag.sourceChainId columns of
                Just sourceColumn ->
                    case Project.nodeIndex joinDrag.sourceNodeId sourceColumn.nodes of
                        Just sourceIndex ->
                            let
                                maybeSourceNode =
                                    Project.findNodeInChain joinDrag.sourceNodeId sourceColumn
                            in
                            case maybeSourceNode of
                                Just sourceNode ->
                                    let
                                        sourcePoint =
                                            joinDragSourcePoint model columns joinDrag sourceColumn sourceIndex sourceNode

                                        targetPoint =
                                            dragTargetPoint model columns joinDrag sourcePoint.x sourcePoint.y
                                    in
                                    [ div
                                        [ style "position" "absolute"
                                        , style "left" "0"
                                        , style "bottom" "0"
                                        , style "width" (px (graphWidth columns))
                                        , style "height" (px (graphHeight model columns))
                                        , style "pointer-events" "none"
                                        , style "z-index" "10"
                                        ]
                                        (viewFloatingDragEdge sourcePoint.x sourcePoint.y targetPoint.x targetPoint.y)
                                    ]

                                Nothing ->
                                    []

                        Nothing ->
                            []

                Nothing ->
                    []

        Nothing ->
            []


dragTargetPoint : Model -> List Project.Chain -> JoinDrag -> Int -> Int -> { x : Int, y : Int }
dragTargetPoint model columns joinDrag fallbackX fallbackY =
    case joinDrag.graphOrigin of
        Just origin ->
            { x = round (joinDrag.mouse.x - origin.x)
            , y = round (toFloat (graphHeight model columns) - (joinDrag.mouse.y - origin.y))
            }

        Nothing ->
            { x = fallbackX, y = fallbackY }


joinDragSourcePoint : Model -> List Project.Chain -> JoinDrag -> Project.Chain -> Int -> Project.Node -> { x : Int, y : Int }
joinDragSourcePoint model columns joinDrag sourceColumn sourceIndex sourceNode =
    case ( joinDrag.graphOrigin, joinDrag.sourceButtonCenter ) of
        ( Just origin, Just buttonCenter ) ->
            { x = round (buttonCenter.x - origin.x)
            , y = round (toFloat (graphHeight model columns) - (buttonCenter.y - origin.y))
            }

        _ ->
            { x = columnX sourceColumn + cardWidth
            , y = nodeCenter model columns sourceColumn sourceIndex sourceNode
            }


viewVerticalEdges : Model -> List Project.Chain -> List (Html Msg)
viewVerticalEdges model columns =
    columns
        |> List.concatMap
            (\column ->
                column.nodes
                    |> List.indexedMap Tuple.pair
                    |> adjacentPairs
                    |> List.concatMap
                        (\( ( sourceIndex, sourceNode ), ( targetIndex, _ ) ) ->
                            viewVerticalEdge
                                (columnX column + (cardWidth // 2))
                                (nodeTop model columns column sourceIndex sourceNode)
                                (nodeBottom model columns column targetIndex)
                        )
            )


viewForkEdges : Model -> List Project.Chain -> List (Html Msg)
viewForkEdges model columns =
    columns
        |> List.concatMap
            (\column ->
                case column.forkedFrom of
                    Just forkRef ->
                        case Project.findChain forkRef.sourceChainId columns of
                            Just sourceColumn ->
                                case ( Project.nodeIndex forkRef.sourceNodeId sourceColumn.nodes, Project.findNodeInChain forkRef.sourceNodeId sourceColumn, column.nodes ) of
                                    ( Just sourceIndex, Just sourceNode, targetNode :: _ ) ->
                                        let
                                            sourceY =
                                                nodeCenter model columns sourceColumn sourceIndex sourceNode

                                            targetY =
                                                nodeCenter model columns column 0 targetNode

                                            x1 =
                                                columnX sourceColumn + cardWidth

                                            x2 =
                                                columnX column
                                        in
                                        viewElbowEdgeRight x1 sourceY x2 targetY

                                    _ ->
                                        []

                            Nothing ->
                                []

                    Nothing ->
                        []
            )


viewJoinEdges : Model -> List Project.Chain -> List (Html Msg)
viewJoinEdges model columns =
    columns
        |> List.concatMap
            (\column ->
                case column.joinedInto of
                    Just joinRef ->
                        case ( Project.lastNode column, Project.findChain joinRef.targetChainId columns ) of
                            ( Just sourceNode, Just targetColumn ) ->
                                case ( Project.nodeIndex sourceNode.id column.nodes, Project.nodeIndex joinRef.targetNodeId targetColumn.nodes, Project.findNodeInChain joinRef.targetNodeId targetColumn ) of
                                    ( Just sourceIndex, Just targetIndex, Just targetNode ) ->
                                        viewInterChainEdge
                                            column
                                            targetColumn
                                            (nodeCenter model columns column sourceIndex sourceNode)
                                            (nodeCenter model columns targetColumn targetIndex targetNode)

                                    _ ->
                                        []

                            _ ->
                                []

                    Nothing ->
                        []
            )


viewInterChainEdge : Project.Chain -> Project.Chain -> Int -> Int -> List (Html Msg)
viewInterChainEdge sourceChain targetChain sourceY targetY =
    let
        sourceColumn =
            sourceChain.order

        targetColumn =
            targetChain.order
    in
    if sourceColumn < targetColumn then
        viewElbowEdgeRight
            (columnX sourceChain + cardWidth)
            sourceY
            (columnX targetChain)
            targetY

    else if sourceColumn > targetColumn then
        viewElbowEdgeLeft
            (columnX sourceChain)
            sourceY
            (columnX targetChain + cardWidth)
            targetY

    else
        viewSameColumnJoinEdge
            (columnX sourceChain + cardWidth)
            sourceY
            (columnX targetChain + cardWidth)
            targetY


viewSameColumnJoinEdge : Int -> Int -> Int -> Int -> List (Html Msg)
viewSameColumnJoinEdge sourceX sourceY targetX targetY =
    let
        routeX =
            sourceX + (createButtonSideOverhang // 2)

        verticalStart =
            min sourceY targetY

        verticalEnd =
            max sourceY targetY

        verticalSegment =
            if verticalEnd == verticalStart then
                []

            else
                [ div
                    [ edgeBase
                    , style "left" (px routeX)
                    , style "bottom" (px verticalStart)
                    , style "height" (px (verticalEnd - verticalStart))
                    , style "border-left" ("1px dashed " ++ edgeColor)
                    ]
                    []
                ]
    in
    horizontalLine sourceX routeX sourceY
        :: (verticalSegment
                ++ [ horizontalLine targetX routeX targetY
                   , arrowLeft targetX targetY
                   ]
           )


viewVerticalEdge : Int -> Int -> Int -> List (Html Msg)
viewVerticalEdge x y1 y2 =
    if y2 <= y1 then
        []

    else
        [ div
            [ edgeBase
            , style "left" (px x)
            , style "bottom" (px y1)
            , style "height" (px (y2 - y1))
            , style "border-left" ("1px dashed " ++ edgeColor)
            ]
            []
        , arrowUp x y2
        ]


viewHorizontalEdgeRight : Int -> Int -> Int -> List (Html Msg)
viewHorizontalEdgeRight x1 x2 y =
    if x2 <= x1 then
        []

    else
        [ horizontalLine x1 x2 y
        , arrowRight x2 y
        ]


viewElbowEdgeLeft : Int -> Int -> Int -> Int -> List (Html Msg)
viewElbowEdgeLeft sourceX sourceY targetX targetY =
    if sourceX <= targetX then
        []

    else
        let
            midX =
                targetX + ((sourceX - targetX) // 2)

            verticalStart =
                min sourceY targetY

            verticalEnd =
                max sourceY targetY

            verticalSegment =
                if verticalEnd == verticalStart then
                    []

                else
                    [ div
                        [ edgeBase
                        , style "left" (px midX)
                        , style "bottom" (px verticalStart)
                        , style "height" (px (verticalEnd - verticalStart))
                        , style "border-left" ("1px dashed " ++ edgeColor)
                        ]
                        []
                    ]
        in
        horizontalLine midX sourceX sourceY
            :: (verticalSegment
                    ++ [ horizontalLine targetX midX targetY
                       , arrowLeft targetX targetY
                       ]
               )


viewElbowEdgeRight : Int -> Int -> Int -> Int -> List (Html Msg)
viewElbowEdgeRight sourceX sourceY targetX targetY =
    if targetX <= sourceX then
        []

    else
        let
            midX =
                sourceX + ((targetX - sourceX) // 2)

            verticalStart =
                min sourceY targetY

            verticalEnd =
                max sourceY targetY

            verticalSegment =
                if verticalEnd == verticalStart then
                    []

                else
                    [ div
                        [ edgeBase
                        , style "left" (px midX)
                        , style "bottom" (px verticalStart)
                        , style "height" (px (verticalEnd - verticalStart))
                        , style "border-left" ("1px dashed " ++ edgeColor)
                        ]
                        []
                    ]
        in
        horizontalLine sourceX midX sourceY
            :: (verticalSegment
                    ++ [ horizontalLine midX targetX targetY
                       , arrowRight targetX targetY
                       ]
               )


viewFloatingDragEdge : Int -> Int -> Int -> Int -> List (Html Msg)
viewFloatingDragEdge sourceX sourceY targetX targetY =
    let
        verticalStart =
            min sourceY targetY

        verticalEnd =
            max sourceY targetY

        verticalSegment =
            if verticalEnd == verticalStart then
                []

            else
                [ div
                    [ edgeBase
                    , style "left" (px targetX)
                    , style "bottom" (px verticalStart)
                    , style "height" (px (verticalEnd - verticalStart))
                    , style "border-left" ("1px dashed " ++ edgeColor)
                    ]
                    []
                ]

        arrow =
            if targetY > sourceY then
                arrowUp targetX targetY

            else if targetY < sourceY then
                arrowDown targetX targetY

            else if targetX < sourceX then
                arrowLeft targetX targetY

            else
                arrowRight targetX targetY
    in
    horizontalLine sourceX targetX sourceY
        :: (verticalSegment ++ [ arrow ])


horizontalLine : Int -> Int -> Int -> Html Msg
horizontalLine x1 x2 y =
    div
        [ edgeBase
        , style "left" (px (min x1 x2))
        , style "bottom" (px y)
        , style "width" (px (abs (x2 - x1)))
        , style "border-top" ("1px dashed " ++ edgeColor)
        ]
        []


arrowUp : Int -> Int -> Html Msg
arrowUp x y =
    div
        [ edgeBase
        , style "left" (px (x - 5))
        , style "bottom" (px (y - 8))
        , style "width" "10px"
        , style "height" "8px"
        , style "background" edgeColor
        , style "clip-path" "polygon(50% 0, 100% 100%, 0 100%)"
        ]
        []


arrowRight : Int -> Int -> Html Msg
arrowRight x y =
    div
        [ edgeBase
        , style "left" (px (x - 8))
        , style "bottom" (px (y - 5))
        , style "width" "8px"
        , style "height" "10px"
        , style "background" edgeColor
        , style "clip-path" "polygon(0 0, 100% 50%, 0 100%)"
        ]
        []


arrowDown : Int -> Int -> Html Msg
arrowDown x y =
    div
        [ edgeBase
        , style "left" (px (x - 5))
        , style "bottom" (px y)
        , style "width" "10px"
        , style "height" "8px"
        , style "background" edgeColor
        , style "clip-path" "polygon(0 0, 100% 0, 50% 100%)"
        ]
        []


arrowLeft : Int -> Int -> Html Msg
arrowLeft x y =
    div
        [ edgeBase
        , style "left" (px x)
        , style "bottom" (px (y - 5))
        , style "width" "8px"
        , style "height" "10px"
        , style "background" edgeColor
        , style "clip-path" "polygon(100% 0, 0 50%, 100% 100%)"
        ]
        []


edgeBase : Html.Attribute Msg
edgeBase =
    style "position" "absolute"


columnX : Project.Chain -> Int
columnX column =
    column.order * columnStep


nodeRowValue : Project.Chain -> Int -> Int
nodeRowValue column index =
    column.baseRow + index


nodeBottom : Model -> List Project.Chain -> Project.Chain -> Int -> Int
nodeBottom model columns column index =
    rowBottom model columns (nodeRowValue column index)


nodeTop : Model -> List Project.Chain -> Project.Chain -> Int -> Project.Node -> Int
nodeTop model columns column index node =
    nodeBottom model columns column index + nodeCardHeight model node


nodeCenter : Model -> List Project.Chain -> Project.Chain -> Int -> Project.Node -> Int
nodeCenter model columns column index node =
    nodeBottom model columns column index + (nodeCardHeight model node // 2)


nodeCardHeight : Model -> Project.Node -> Int
nodeCardHeight model node =
    model.nodeHeights
        |> List.filter (.nodeId >> (==) node.id)
        |> List.head
        |> Maybe.map .height
        |> Maybe.withDefault nodeHeight


nodeGridRow : Int -> Project.Chain -> Int -> Int
nodeGridRow maxRow column index =
    maxRow - nodeRowValue column index + 1


columnTopRow : Project.Chain -> Int
columnTopRow column =
    column.baseRow + max 0 (List.length column.nodes - 1)


maxGraphRow : List Project.Chain -> Int
maxGraphRow columns =
    columns
        |> List.map columnTopRow
        |> List.maximum
        |> Maybe.withDefault 0


graphRows : List Project.Chain -> List Int
graphRows columns =
    List.range 0 (maxGraphRow columns)


rowHeight : Model -> List Project.Chain -> Int -> Int
rowHeight model columns row =
    columns
        |> List.concatMap
            (\column ->
                column.nodes
                    |> List.indexedMap
                        (\index node ->
                            if nodeRowValue column index == row then
                                Just (nodeCardHeight model node)

                            else
                                Nothing
                        )
                    |> List.filterMap identity
            )
        |> List.maximum
        |> Maybe.withDefault nodeHeight


rowBottom : Model -> List Project.Chain -> Int -> Int
rowBottom model columns row =
    if row <= 0 then
        0

    else
        List.range 0 (row - 1)
            |> List.map (\lowerRow -> rowHeight model columns lowerRow + rowGap)
            |> List.sum


gridTemplateRows : Model -> List Project.Chain -> String
gridTemplateRows model columns =
    graphRows columns
        |> List.reverse
        |> List.map (rowHeight model columns >> px)
        |> String.join " "


columnCount : List Project.Chain -> Int
columnCount columns =
    let
        maxOrder =
            columns
                |> List.map .order
                |> List.maximum
                |> Maybe.withDefault 0
    in
    maxOrder + 1


graphWidth : List Project.Chain -> Int
graphWidth columns =
    let
        maxOrder =
            columnCount columns - 1
    in
    (maxOrder * columnStep) + nodeGridWidth + max 24 createButtonSideOverhang


graphHeight : Model -> List Project.Chain -> Int
graphHeight model columns =
    let
        rows =
            graphRows columns
    in
    rows
        |> List.map (rowHeight model columns)
        |> List.sum
        |> (+) ((rowGap * max 0 (List.length rows - 1)) + createButtonOverhang)


adjacentPairs : List a -> List ( a, a )
adjacentPairs list =
    case list of
        first :: second :: rest ->
            ( first, second ) :: adjacentPairs (second :: rest)

        _ ->
            []


px : Int -> String
px value =
    String.fromInt value ++ "px"


joinPointerDecoder : Decode.Decoder JoinPointer
joinPointerDecoder =
    Decode.map3
        (\pointerId x y ->
            { id = pointerId
            , point = { x = x, y = y }
            }
        )
        (Decode.field "pointerId" Decode.int)
        (Decode.field "pageX" Decode.float)
        (Decode.field "pageY" Decode.float)


mouseJoinPointerDecoder : Decode.Decoder JoinPointer
mouseJoinPointerDecoder =
    Decode.map2
        (\x y ->
            { id = mouseFallbackPointerId
            , point = { x = x, y = y }
            }
        )
        (Decode.field "pageX" Decode.float)
        (Decode.field "pageY" Decode.float)


shortUuid : UUID -> String
shortUuid id =
    UUID.toString id
        |> String.left 8
