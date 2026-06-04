module Page.TodoGraph exposing (..)

import Browser exposing (Document)
import Browser.Dom as Dom
import Browser.Events as BrowserEvents
import Control.TodoGraphItem as TodoGraphItem
import Domain.Project as Project
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (attribute, class, id, style, title)
import Html.Events exposing (on, onClick, onMouseEnter, onMouseLeave)
import Json.Decode as Decode
import Platform.Cmd as Cmd
import Random
import Task
import Time
import UUID exposing (UUID)


type alias Model =
    { project : Project.Project
    , nodeUiStates : List NodeUiState
    , joinDrag : Maybe JoinDrag
    }


type alias NodeUiState =
    { nodeId : UUID
    , textEditState : TodoGraphItem.TextEditState
    }


type alias JoinDrag =
    { sourceColumnId : UUID
    , sourceNodeId : UUID
    , mouse : MousePoint
    , graphOrigin : Maybe MousePoint
    , hoveredNodeId : Maybe UUID
    }


type alias MousePoint =
    { x : Float
    , y : Float
    }


type Msg
    = GraphItemMsg UUID TodoGraphItem.Msg
    | CreateTextNode UUID
    | CreateTimerNode UUID
    | TextNodeGenerated UUID UUID
    | TimerNodeGenerated UUID UUID
    | CreateFork UUID UUID
    | ForkGenerated UUID UUID UUID UUID
    | StartJoinDrag UUID UUID MousePoint
    | GraphElementMeasured (Result Dom.Error Dom.Element)
    | MoveJoinDrag MousePoint
    | FinishJoinDrag MousePoint
    | HoverJoinTarget UUID
    | LeaveJoinTarget UUID
    | DeleteNode UUID
    | Tick Time.Posix


cardWidth : Int
cardWidth =
    280


nodeHeight : Int
nodeHeight =
    58


rowStep : Int
rowStep =
    92


columnStep : Int
columnStep =
    380


forkButtonSize : Int
forkButtonSize =
    46


forkButtonGap : Int
forkButtonGap =
    10


nodeGridWidth : Int
nodeGridWidth =
    cardWidth + forkButtonGap + (forkButtonSize * 2) + 8


columnGap : Int
columnGap =
    columnStep - cardWidth


rowGap : Int
rowGap =
    rowStep - nodeHeight


deleteButtonSize : Int
deleteButtonSize =
    26


toolbarHeight : Int
toolbarHeight =
    32


toolbarGap : Int
toolbarGap =
    12


edgeColor : String
edgeColor =
    "#61666d"


graphRootId : String
graphRootId =
    "todo-graph-root"


init : Model -> ( Model, Cmd Msg )
init model =
    ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GraphItemMsg nodeId graphItemMsg ->
            updateGraphItem nodeId graphItemMsg model

        CreateTextNode columnId ->
            ( model
            , Random.generate (TextNodeGenerated columnId) UUID.generator
            )

        CreateTimerNode columnId ->
            ( model
            , Random.generate (TimerNodeGenerated columnId) UUID.generator
            )

        TextNodeGenerated columnId nodeId ->
            ( { model
                | project =
                    model.project
                        |> Project.appendNode columnId (Project.textNode nodeId "item")
              }
            , Cmd.none
            )

        TimerNodeGenerated columnId nodeId ->
            ( { model
                | project =
                    model.project
                        |> Project.appendNode columnId (Project.timerNode nodeId "timer")
              }
            , Cmd.none
            )

        CreateFork sourceColumnId sourceNodeId ->
            ( model
            , Random.generate
                (\( columnId, nodeId ) ->
                    ForkGenerated sourceColumnId sourceNodeId columnId nodeId
                )
                (Random.map2 Tuple.pair UUID.generator UUID.generator)
            )

        ForkGenerated sourceColumnId sourceNodeId columnId nodeId ->
            ( { model
                | project =
                    model.project
                        |> Project.forkColumn
                            sourceColumnId
                            sourceNodeId
                            columnId
                            (Project.textNode nodeId "item")
              }
            , Cmd.none
            )

        StartJoinDrag sourceColumnId sourceNodeId mouse ->
            ( { model
                | joinDrag =
                    Just
                        { sourceColumnId = sourceColumnId
                        , sourceNodeId = sourceNodeId
                        , mouse = mouse
                        , graphOrigin = Nothing
                        , hoveredNodeId = Nothing
                        }
              }
            , Dom.getElement graphRootId
                |> Task.attempt GraphElementMeasured
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

        MoveJoinDrag mouse ->
            ( { model
                | joinDrag =
                    model.joinDrag
                        |> Maybe.map (\joinDrag -> { joinDrag | mouse = mouse })
              }
            , Cmd.none
            )

        FinishJoinDrag mouse ->
            case model.joinDrag of
                Just joinDrag ->
                    let
                        nextProject =
                            joinDrag.hoveredNodeId
                                |> Maybe.map
                                    (\targetNodeId ->
                                        Project.joinColumnToNode joinDrag.sourceColumnId targetNodeId model.project
                                    )
                                |> Maybe.withDefault model.project
                    in
                    ( { model | project = nextProject, joinDrag = Nothing }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        HoverJoinTarget nodeId ->
            ( { model
                | joinDrag =
                    model.joinDrag
                        |> Maybe.map
                            (\joinDrag ->
                                if Project.canJoinColumnToNode joinDrag.sourceColumnId nodeId model.project then
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
            ( { model
                | project = Project.deleteNode nodeId model.project
                , nodeUiStates =
                    model.nodeUiStates
                        |> List.filter (.nodeId >> (/=) nodeId)
              }
            , Cmd.none
            )

        Tick _ ->
            ( { model | project = Project.advanceRunningTimers 1 model.project }
            , Cmd.none
            )


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
                    [ BrowserEvents.onMouseMove (Decode.map MoveJoinDrag mousePointDecoder)
                    , BrowserEvents.onMouseUp (Decode.map FinishJoinDrag mousePointDecoder)
                    ]

            Nothing ->
                Sub.none
        ]


hasRunningTimer : Project.Project -> Bool
hasRunningTimer project =
    project.columns
        |> List.concatMap .nodes
        |> List.any
            (\node ->
                node.timer
                    |> Maybe.map .running
                    |> Maybe.withDefault False
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
            in
            ( { model
                | project =
                    model.project
                        |> Project.updateNode nodeId updatedControlModel.node
                , nodeUiStates =
                    upsertNodeUiState updatedControlModel model.nodeUiStates
              }
            , Cmd.map (GraphItemMsg nodeId) command
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


findNode : UUID -> Project.Project -> Maybe Project.Node
findNode nodeId project =
    project.columns
        |> List.concatMap .nodes
        |> List.filter (.id >> (==) nodeId)
        |> List.head


view : Model -> Document Msg
view model =
    let
        sortedColumns =
            model.project.columns
                |> Project.sortColumns

        maxRow =
            maxGraphRow sortedColumns

        hideButtons =
            model.joinDrag /= Nothing
    in
    { title = Maybe.withDefault ("Project " ++ shortUuid model.project.id) model.project.name
    , body =
        [ div
            [ style "min-height" "calc(100vh - 72px)"
            , style "overflow" "auto"
            , style "background" "#fff"
            , style "display" "flex"
            , style "align-items" "flex-end"
            , style "justify-content" "flex-start"
            ]
            [ div
                [ id graphRootId
                , style "position" "relative"
                , style "display" "grid"
                , style "grid-template-columns" ("repeat(" ++ String.fromInt (columnCount sortedColumns) ++ ", " ++ px cardWidth ++ ")")
                , style "grid-template-rows" ("repeat(" ++ String.fromInt (maxRow + 1) ++ ", " ++ px nodeHeight ++ ")")
                , style "column-gap" (px columnGap)
                , style "row-gap" (px rowGap)
                , style "align-content" "end"
                , style "justify-content" "start"
                , style "width" (px (graphWidth sortedColumns))
                , style "height" (px (graphHeight sortedColumns))
                , style "min-width" (px (graphWidth sortedColumns))
                , style "min-height" (px (graphHeight sortedColumns))
                ]
                (viewEdges sortedColumns
                    ++ viewDragEdge sortedColumns model.joinDrag
                    ++ viewToolbars hideButtons maxRow sortedColumns
                    ++ viewNodes hideButtons maxRow model sortedColumns
                )
            ]
        ]
    }


viewToolbars : Bool -> Int -> List Project.Column -> List (Html Msg)
viewToolbars hideButtons maxRow columns =
    if hideButtons then
        []

    else
        columns
            |> List.map (viewColumnToolbar maxRow)


viewColumnToolbar : Int -> Project.Column -> Html Msg
viewColumnToolbar maxRow column =
    div
        [ class "d-flex align-items-center gap-2"
        , style "grid-column" (String.fromInt (column.order + 1))
        , style "grid-row" (String.fromInt (toolbarGridRow maxRow column))
        , style "width" (px cardWidth)
        , style "height" (px toolbarHeight)
        , style "align-self" "start"
        , style "justify-self" "start"
        , style "transform" ("translateY(-" ++ px (toolbarHeight + toolbarGap) ++ ")")
        , style "z-index" "2"
        ]
        [ button
            [ onClick (CreateTextNode column.id), class "btn btn-sm btn-outline-dark" ]
            [ text "New item" ]
        , button
            [ onClick (CreateTimerNode column.id), class "btn btn-sm btn-outline-dark" ]
            [ text "New timer" ]
        ]


viewNodes : Bool -> Int -> Model -> List Project.Column -> List (Html Msg)
viewNodes hideButtons maxRow model columns =
    columns
        |> List.concatMap
            (\column ->
                column.nodes
                    |> List.indexedMap (viewNode hideButtons maxRow model column)
            )


viewNode : Bool -> Int -> Model -> Project.Column -> Int -> Project.Node -> Html Msg
viewNode hideButtons maxRow model column index node =
    let
        itemHtml =
            TodoGraphItem.view { hideButtons = hideButtons } (controlModelFor node model)
                |> Html.map (GraphItemMsg node.id)
    in
    div
        [ style "position" "relative"
        , style "grid-column" (String.fromInt (column.order + 1))
        , style "grid-row" (String.fromInt (nodeGridRow maxRow column index))
        , style "width" (px nodeGridWidth)
        , style "height" (px nodeHeight)
        , style "align-self" "stretch"
        , style "justify-self" "start"
        , style "z-index" "3"
        , onMouseEnter (HoverJoinTarget node.id)
        , onMouseLeave (LeaveJoinTarget node.id)
        ]
        ([ div
            ([ style "position" "absolute"
             , style "left" "0"
             , style "top" "0"
             , style "width" (px cardWidth)
             , style "height" (px nodeHeight)
             ]
                ++ joinTargetStyles model.project model.joinDrag column node
            )
            [ itemHtml ]
         ]
            ++ viewDeleteNodeButton hideButtons model.project node
            ++ viewJoinHandle hideButtons column index node
            ++ viewForkButton hideButtons column node
        )


viewForkButton : Bool -> Project.Column -> Project.Node -> List (Html Msg)
viewForkButton hideButtons column node =
    if hideButtons then
        []

    else
        [ button
            [ onClick (CreateFork column.id node.id)
            , class "btn btn-sm btn-outline-dark rounded-circle"
            , style "position" "absolute"
            , style "left" (px (cardWidth + forkButtonGap))
            , style "top" (px ((nodeHeight - forkButtonSize) // 2))
            , style "width" (px forkButtonSize)
            , style "height" (px forkButtonSize)
            , style "z-index" "3"
            ]
            [ text "fork" ]
        ]


viewJoinHandle : Bool -> Project.Column -> Int -> Project.Node -> List (Html Msg)
viewJoinHandle hideButtons column index node =
    if hideButtons || not (canStartJoinDrag column index) then
        []

    else
        [ button
            [ on "mousedown" (Decode.map (StartJoinDrag column.id node.id) mousePointDecoder)
            , class "btn btn-sm btn-outline-primary rounded-circle"
            , title "Join"
            , attribute "aria-label" "Join"
            , style "position" "absolute"
            , style "left" (px (cardWidth + forkButtonGap + forkButtonSize + 8))
            , style "top" (px ((nodeHeight - forkButtonSize) // 2))
            , style "width" (px forkButtonSize)
            , style "height" (px forkButtonSize)
            , style "z-index" "3"
            ]
            [ text "join" ]
        ]


canStartJoinDrag : Project.Column -> Int -> Bool
canStartJoinDrag column index =
    (column.forkedFrom /= Nothing)
        && (column.joinedInto == Nothing)
        && (index == List.length column.nodes - 1)


viewDeleteNodeButton : Bool -> Project.Project -> Project.Node -> List (Html Msg)
viewDeleteNodeButton hideButtons project node =
    if hideButtons then
        []

    else if Project.canDeleteNode node.id project then
        [ button
            [ onClick (DeleteNode node.id)
            , class "btn btn-sm btn-outline-danger rounded-circle"
            , title "Delete node"
            , attribute "aria-label" "Delete node"
            , style "position" "absolute"
            , style "right" (px (nodeGridWidth - cardWidth + 6))
            , style "top" "6px"
            , style "width" (px deleteButtonSize)
            , style "height" (px deleteButtonSize)
            , style "line-height" "1"
            , style "padding" "0"
            , style "z-index" "4"
            ]
            [ text "x" ]
        ]

    else
        []


joinTargetStyles : Project.Project -> Maybe JoinDrag -> Project.Column -> Project.Node -> List (Html.Attribute Msg)
joinTargetStyles project maybeJoinDrag column node =
    case maybeJoinDrag of
        Just joinDrag ->
            if node.id == joinDrag.sourceNodeId then
                []

            else if Project.canJoinColumnToNode joinDrag.sourceColumnId node.id project then
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


viewEdges : List Project.Column -> List (Html Msg)
viewEdges columns =
    viewVerticalEdges columns
        ++ viewForkEdges columns
        ++ viewJoinEdges columns


viewDragEdge : List Project.Column -> Maybe JoinDrag -> List (Html Msg)
viewDragEdge columns maybeJoinDrag =
    case maybeJoinDrag of
        Just joinDrag ->
            case Project.findColumn joinDrag.sourceColumnId columns of
                Just sourceColumn ->
                    case Project.nodeRow sourceColumn joinDrag.sourceNodeId of
                        Just sourceRow ->
                            let
                                sourceX =
                                    columnX sourceColumn + cardWidth

                                sourceY =
                                    rowCenter sourceRow

                                targetPoint =
                                    dragTargetPoint columns joinDrag sourceX sourceY
                            in
                            [ div
                                [ style "position" "absolute"
                                , style "left" "0"
                                , style "bottom" "0"
                                , style "width" (px (graphWidth columns))
                                , style "height" (px (graphHeight columns))
                                , style "pointer-events" "none"
                                , style "z-index" "10"
                                ]
                                (viewFloatingDragEdge sourceX sourceY targetPoint.x targetPoint.y)
                            ]

                        Nothing ->
                            []

                Nothing ->
                    []

        Nothing ->
            []


dragTargetPoint : List Project.Column -> JoinDrag -> Int -> Int -> { x : Int, y : Int }
dragTargetPoint columns joinDrag fallbackX fallbackY =
    case joinDrag.graphOrigin of
        Just origin ->
            { x = round (joinDrag.mouse.x - origin.x)
            , y = round (toFloat (graphHeight columns) - (joinDrag.mouse.y - origin.y))
            }

        Nothing ->
            { x = fallbackX, y = fallbackY }


viewVerticalEdges : List Project.Column -> List (Html Msg)
viewVerticalEdges columns =
    columns
        |> List.concatMap
            (\column ->
                column.nodes
                    |> List.indexedMap Tuple.pair
                    |> adjacentPairs
                    |> List.concatMap
                        (\( ( sourceIndex, _ ), ( targetIndex, _ ) ) ->
                            viewVerticalEdge
                                (columnX column + (cardWidth // 2))
                                (nodeBottom column sourceIndex + nodeHeight)
                                (nodeBottom column targetIndex)
                        )
            )


viewForkEdges : List Project.Column -> List (Html Msg)
viewForkEdges columns =
    columns
        |> List.concatMap
            (\column ->
                case column.forkedFrom of
                    Just forkRef ->
                        case Project.findColumn forkRef.sourceColumnId columns of
                            Just sourceColumn ->
                                case Project.nodeRow sourceColumn forkRef.sourceNodeId of
                                    Just sourceRow ->
                                        let
                                            y =
                                                rowCenter sourceRow

                                            x1 =
                                                columnX sourceColumn + cardWidth

                                            x2 =
                                                columnX column
                                        in
                                        viewHorizontalEdgeRight x1 x2 y

                                    Nothing ->
                                        []

                            Nothing ->
                                []

                    Nothing ->
                        []
            )


viewJoinEdges : List Project.Column -> List (Html Msg)
viewJoinEdges columns =
    columns
        |> List.concatMap
            (\column ->
                case column.joinedInto of
                    Just joinRef ->
                        case ( Project.lastNode column, Project.findColumn joinRef.targetColumnId columns ) of
                            ( Just sourceNode, Just targetColumn ) ->
                                case ( Project.nodeRow column sourceNode.id, Project.nodeRow targetColumn joinRef.targetNodeId ) of
                                    ( Just sourceRow, Just targetRow ) ->
                                        viewElbowEdgeLeft
                                            (columnX column)
                                            (rowCenter sourceRow)
                                            (columnX targetColumn + cardWidth)
                                            (rowCenter targetRow)

                                    _ ->
                                        []

                            _ ->
                                []

                    Nothing ->
                        []
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
        , style "bottom" (px (y - 1))
        , style "width" "0"
        , style "height" "0"
        , style "border-left" "5px solid transparent"
        , style "border-right" "5px solid transparent"
        , style "border-bottom" ("8px solid " ++ edgeColor)
        ]
        []


arrowRight : Int -> Int -> Html Msg
arrowRight x y =
    div
        [ edgeBase
        , style "left" (px (x - 1))
        , style "bottom" (px (y - 5))
        , style "width" "0"
        , style "height" "0"
        , style "border-top" "5px solid transparent"
        , style "border-bottom" "5px solid transparent"
        , style "border-left" ("8px solid " ++ edgeColor)
        ]
        []


arrowDown : Int -> Int -> Html Msg
arrowDown x y =
    div
        [ edgeBase
        , style "left" (px (x - 5))
        , style "bottom" (px (y - 7))
        , style "width" "0"
        , style "height" "0"
        , style "border-left" "5px solid transparent"
        , style "border-right" "5px solid transparent"
        , style "border-top" ("8px solid " ++ edgeColor)
        ]
        []


arrowLeft : Int -> Int -> Html Msg
arrowLeft x y =
    div
        [ edgeBase
        , style "left" (px (x - 7))
        , style "bottom" (px (y - 5))
        , style "width" "0"
        , style "height" "0"
        , style "border-top" "5px solid transparent"
        , style "border-bottom" "5px solid transparent"
        , style "border-right" ("8px solid " ++ edgeColor)
        ]
        []


edgeBase : Html.Attribute Msg
edgeBase =
    style "position" "absolute"


columnX : Project.Column -> Int
columnX column =
    column.order * columnStep


nodeRowValue : Project.Column -> Int -> Int
nodeRowValue column index =
    column.baseRow + index


nodeBottom : Project.Column -> Int -> Int
nodeBottom column index =
    nodeRowValue column index * rowStep


nodeGridRow : Int -> Project.Column -> Int -> Int
nodeGridRow maxRow column index =
    maxRow - nodeRowValue column index + 1


toolbarGridRow : Int -> Project.Column -> Int
toolbarGridRow maxRow column =
    maxRow - columnTopRow column + 1


rowCenter : Int -> Int
rowCenter row =
    (row * rowStep) + (nodeHeight // 2)


columnTopRow : Project.Column -> Int
columnTopRow column =
    column.baseRow + max 0 (List.length column.nodes - 1)


maxGraphRow : List Project.Column -> Int
maxGraphRow columns =
    columns
        |> List.map columnTopRow
        |> List.maximum
        |> Maybe.withDefault 0


columnCount : List Project.Column -> Int
columnCount columns =
    let
        maxOrder =
            columns
                |> List.map .order
                |> List.maximum
                |> Maybe.withDefault 0
    in
    maxOrder + 1


graphWidth : List Project.Column -> Int
graphWidth columns =
    let
        maxOrder =
            columnCount columns - 1
    in
    (maxOrder * columnStep) + nodeGridWidth + 24


graphHeight : List Project.Column -> Int
graphHeight columns =
    (maxGraphRow columns * rowStep) + nodeHeight + toolbarGap + toolbarHeight


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


mousePointDecoder : Decode.Decoder MousePoint
mousePointDecoder =
    Decode.map2
        (\x y -> { x = x, y = y })
        (Decode.field "pageX" Decode.float)
        (Decode.field "pageY" Decode.float)


shortUuid : UUID -> String
shortUuid id =
    UUID.toString id
        |> String.left 8
