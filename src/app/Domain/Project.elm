module Domain.Project exposing (..)

import Time
import UUID exposing (UUID)


type Timer
    = Started Time.Posix
    | Stopped Int


type alias Node =
    { id : UUID
    , text : String
    , status : Bool
    , timer : Maybe Timer
    , description : Maybe String
    }


type alias ForkRef =
    { sourceColumnId : UUID
    , sourceNodeId : UUID
    }


type alias JoinRef =
    { targetColumnId : UUID
    , targetNodeId : UUID
    }


type alias Column =
    { id : UUID
    , order : Int
    , baseRow : Int
    , name : Maybe String
    , nodes : List Node
    , forkedFrom : Maybe ForkRef
    , joinedInto : Maybe JoinRef
    }


type alias Project =
    { id : UUID
    , name : Maybe String
    , columns : List Column
    }


textNode : UUID -> String -> Node
textNode id text =
    { id = id
    , text = text
    , status = False
    , timer = Nothing
    , description = Nothing
    }


timerNode : UUID -> String -> Node
timerNode id text =
    { id = id
    , text = text
    , status = False
    , timer = Just (Stopped 0)
    , description = Nothing
    }


initialProject : UUID -> UUID -> Project
initialProject projectId firstNodeId =
    { id = projectId
    , name = Nothing
    , columns =
        [ { id = projectId
          , order = 0
          , baseRow = 0
          , name = Nothing
          , nodes = [ textNode firstNodeId "item" ]
          , forkedFrom = Nothing
          , joinedInto = Nothing
          }
        ]
    }


appendNode : UUID -> Node -> Project -> Project
appendNode columnId node project =
    { project
        | columns =
            project.columns
                |> List.map
                    (\column ->
                        if column.id == columnId then
                            { column | nodes = column.nodes ++ [ node ] }

                        else
                            column
                    )
    }


insertNodeAfter : UUID -> Node -> Project -> Project
insertNodeAfter afterNodeId newNode project =
    { project
        | columns =
            project.columns
                |> List.map
                    (\column ->
                        { column
                            | nodes =
                                column.nodes
                                    |> insertNodeAfterInList afterNodeId newNode
                        }
                    )
    }
        |> realignForkColumnBaseRows


insertNodeAfterInList : UUID -> Node -> List Node -> List Node
insertNodeAfterInList afterNodeId newNode nodes =
    case nodes of
        node :: rest ->
            if node.id == afterNodeId then
                node :: newNode :: rest

            else
                node :: insertNodeAfterInList afterNodeId newNode rest

        [] ->
            []


updateNode : UUID -> Node -> Project -> Project
updateNode nodeId updatedNode project =
    { project
        | columns =
            project.columns
                |> List.map
                    (\column ->
                        { column
                            | nodes =
                                column.nodes
                                    |> List.map
                                        (\node ->
                                            if node.id == nodeId then
                                                updatedNode

                                            else
                                                node
                                        )
                        }
                    )
    }


addTimerToNode : UUID -> Project -> Project
addTimerToNode nodeId project =
    updateNodeIf nodeId
        (\node ->
            case node.timer of
                Just _ ->
                    node

                Nothing ->
                    { node | timer = Just (Stopped 0) }
        )
        project


addDescriptionToNode : UUID -> Project -> Project
addDescriptionToNode nodeId project =
    updateNodeIf nodeId
        (\node ->
            case node.description of
                Just _ ->
                    node

                Nothing ->
                    { node | description = Just "" }
        )
        project


updateNodeIf : UUID -> (Node -> Node) -> Project -> Project
updateNodeIf nodeId update project =
    { project
        | columns =
            project.columns
                |> List.map
                    (\column ->
                        { column
                            | nodes =
                                column.nodes
                                    |> List.map
                                        (\node ->
                                            if node.id == nodeId then
                                                update node

                                            else
                                                node
                                        )
                        }
                    )
    }


forkColumn : UUID -> UUID -> UUID -> Node -> Project -> Project
forkColumn sourceColumnId sourceNodeId newColumnId firstNode project =
    let
        sourceColumn =
            findColumn sourceColumnId project.columns

        sourceOrder =
            sourceColumn
                |> Maybe.map .order
                |> Maybe.withDefault 0

        sourceRow =
            sourceColumn
                |> Maybe.map
                    (\column ->
                        column.baseRow
                            + Maybe.withDefault 0 (nodeIndex sourceNodeId column.nodes)
                    )
                |> Maybe.withDefault 0

        shiftedColumns =
            project.columns
                |> List.map
                    (\column ->
                        if column.order > sourceOrder then
                            { column | order = column.order + 1 }

                        else
                            column
                    )

        newColumn =
            { id = newColumnId
            , order = sourceOrder + 1
            , baseRow = sourceRow
            , name = Nothing
            , nodes = [ firstNode ]
            , forkedFrom =
                Just
                    { sourceColumnId = sourceColumnId
                    , sourceNodeId = sourceNodeId
                    }
            , joinedInto = Nothing
            }
    in
    { project | columns = sortColumns (newColumn :: shiftedColumns) }


joinColumnIntoPrevious : UUID -> Project -> Project
joinColumnIntoPrevious columnId project =
    case findColumn columnId project.columns of
        Just column ->
            project.columns
                |> List.filter (\candidate -> candidate.order < column.order)
                |> sortColumns
                |> List.reverse
                |> List.head
                |> Maybe.andThen
                    (\targetColumn ->
                        lastNode targetColumn
                            |> Maybe.map (\targetNode -> ( targetColumn, targetNode ))
                    )
                |> Maybe.map
                    (\( targetColumn, targetNode ) ->
                        { project
                            | columns =
                                project.columns
                                    |> List.map
                                        (\candidate ->
                                            if candidate.id == columnId then
                                                { candidate
                                                    | joinedInto =
                                                        Just
                                                            { targetColumnId = targetColumn.id
                                                            , targetNodeId = targetNode.id
                                                            }
                                                }

                                            else
                                                candidate
                                        )
                        }
                    )
                |> Maybe.withDefault project

        Nothing ->
            project


joinColumnToNode : UUID -> UUID -> Project -> Project
joinColumnToNode sourceColumnId targetNodeId project =
    if canJoinColumnToNode sourceColumnId targetNodeId project then
        case locateNode targetNodeId project of
            Just ( targetColumn, targetNode ) ->
                { project
                    | columns =
                        project.columns
                            |> List.map
                                (\column ->
                                    if column.id == sourceColumnId then
                                        { column
                                            | joinedInto =
                                                Just
                                                    { targetColumnId = targetColumn.id
                                                    , targetNodeId = targetNode.id
                                                    }
                                        }

                                    else
                                        column
                                )
                }

            Nothing ->
                project

    else
        project


unjoinColumn : UUID -> Project -> Project
unjoinColumn columnId project =
    { project
        | columns =
            project.columns
                |> List.map
                    (\column ->
                        if column.id == columnId then
                            { column | joinedInto = Nothing }

                        else
                            column
                    )
    }


canJoinColumnToNode : UUID -> UUID -> Project -> Bool
canJoinColumnToNode sourceColumnId targetNodeId project =
    case ( findColumn sourceColumnId project.columns, locateNode targetNodeId project ) of
        ( Just sourceColumn, Just ( targetColumn, _ ) ) ->
            case lastNode sourceColumn of
                Just sourceNode ->
                    sourceColumn.id
                        /= targetColumn.id
                        && sourceColumn.joinedInto
                        == Nothing
                        && not (isReachable targetNodeId sourceNode.id project)

                Nothing ->
                    False

        _ ->
            False


canCreateAfterNode : UUID -> Project -> Bool
canCreateAfterNode nodeId project =
    not (isJoinSourceInProject nodeId project)


isReachable : UUID -> UUID -> Project -> Bool
isReachable sourceNodeId targetNodeId project =
    reachableFrom [ sourceNodeId ] [] targetNodeId project


reachableFrom : List UUID -> List UUID -> UUID -> Project -> Bool
reachableFrom pending visited targetNodeId project =
    case pending of
        currentNodeId :: rest ->
            if currentNodeId == targetNodeId then
                True

            else if List.member currentNodeId visited then
                reachableFrom rest visited targetNodeId project

            else
                let
                    nextNodes =
                        directedNeighborNodeIds currentNodeId project
                            |> List.filter (\nodeId -> not (List.member nodeId visited))
                in
                reachableFrom (rest ++ nextNodes) (currentNodeId :: visited) targetNodeId project

        [] ->
            False


directedNeighborNodeIds : UUID -> Project -> List UUID
directedNeighborNodeIds nodeId project =
    directedEdges project
        |> List.filterMap
            (\( sourceNodeId, targetNodeId ) ->
                if sourceNodeId == nodeId then
                    Just targetNodeId

                else
                    Nothing
            )


directedEdges : Project -> List ( UUID, UUID )
directedEdges project =
    project.columns
        |> List.concatMap columnDirectedEdges


columnDirectedEdges : Column -> List ( UUID, UUID )
columnDirectedEdges column =
    verticalDirectedEdges column.nodes
        ++ forkDirectedEdge column
        ++ joinDirectedEdge column


verticalDirectedEdges : List Node -> List ( UUID, UUID )
verticalDirectedEdges nodes =
    case nodes of
        source :: target :: rest ->
            ( source.id, target.id ) :: verticalDirectedEdges (target :: rest)

        _ ->
            []


forkDirectedEdge : Column -> List ( UUID, UUID )
forkDirectedEdge column =
    case ( column.forkedFrom, column.nodes ) of
        ( Just forkRef, firstNode :: _ ) ->
            [ ( forkRef.sourceNodeId, firstNode.id ) ]

        _ ->
            []


joinDirectedEdge : Column -> List ( UUID, UUID )
joinDirectedEdge column =
    case ( column.joinedInto, lastNode column ) of
        ( Just joinRef, Just sourceNode ) ->
            [ ( sourceNode.id, joinRef.targetNodeId ) ]

        _ ->
            []


canDeleteNode : UUID -> Project -> Bool
canDeleteNode nodeId project =
    case locateNode nodeId project of
        Just ( column, _ ) ->
            if canDeleteForkEntryNode nodeId column project then
                True

            else if isJoinSource nodeId column then
                not (isForkSource nodeId project)
                    && not (isForkEntryNode nodeId column)
                    && not (isJoinTarget nodeId project)

            else
                not (hasInterColumnEdge nodeId project)

        Nothing ->
            False


deleteNode : UUID -> Project -> Project
deleteNode nodeId project =
    case locateNode nodeId project of
        Just ( column, _ ) ->
            if canDeleteForkEntryNode nodeId column project then
                deleteColumn column.id project

            else if isJoinSource nodeId column then
                if canDeleteNode nodeId project then
                    deleteNodeFromColumn True column.id nodeId project

                else
                    project

            else if canDeleteNode nodeId project then
                deleteNodeFromColumn False column.id nodeId project

            else
                project

        Nothing ->
            project


deleteNodeFromColumn : Bool -> UUID -> UUID -> Project -> Project
deleteNodeFromColumn clearJoin columnId nodeId project =
    { project
        | columns =
            project.columns
                |> List.map
                    (\column ->
                        if column.id == columnId then
                            { column
                                | nodes =
                                    column.nodes
                                        |> List.filter (.id >> (/=) nodeId)
                                , joinedInto =
                                    if clearJoin then
                                        Nothing

                                    else
                                        column.joinedInto
                            }

                        else
                            column
                    )
    }
        |> realignForkColumnBaseRows


deleteColumn : UUID -> Project -> Project
deleteColumn columnId project =
    { project
        | columns =
            project.columns
                |> List.filter (.id >> (/=) columnId)
                |> normalizeColumnOrders
    }
        |> realignForkColumnBaseRows


realignForkColumnBaseRows : Project -> Project
realignForkColumnBaseRows project =
    let
        alignColumns remainingColumns alignedColumns =
            case remainingColumns of
                column :: rest ->
                    let
                        alignedColumn =
                            case column.forkedFrom of
                                Just forkRef ->
                                    alignedColumns
                                        |> findColumn forkRef.sourceColumnId
                                        |> Maybe.andThen (\sourceColumn -> nodeRow sourceColumn forkRef.sourceNodeId)
                                        |> Maybe.map (\sourceRow -> { column | baseRow = sourceRow })
                                        |> Maybe.withDefault column

                                Nothing ->
                                    column
                    in
                    alignColumns rest (alignedColumn :: alignedColumns)

                [] ->
                    alignedColumns
                        |> List.reverse
    in
    { project | columns = alignColumns (sortColumns project.columns) [] }


normalizeColumnOrders : List Column -> List Column
normalizeColumnOrders columns =
    columns
        |> sortColumns
        |> List.indexedMap (\index column -> { column | order = index })


hasInterColumnEdge : UUID -> Project -> Bool
hasInterColumnEdge nodeId project =
    isForkSource nodeId project
        || isForkTarget nodeId project
        || isJoinSourceInProject nodeId project
        || isJoinTarget nodeId project


canDeleteForkEntryNode : UUID -> Column -> Project -> Bool
canDeleteForkEntryNode nodeId column project =
    isForkEntryNode nodeId column
        && (List.length column.nodes == 1)
        && not (isForkSource nodeId project)
        && not (isJoinTarget nodeId project)


isForkEntryNode : UUID -> Column -> Bool
isForkEntryNode nodeId column =
    case ( column.forkedFrom, column.nodes ) of
        ( Just _, firstNode :: _ ) ->
            firstNode.id == nodeId

        _ ->
            False


isForkSource : UUID -> Project -> Bool
isForkSource nodeId project =
    project.columns
        |> List.any
            (\column ->
                column.forkedFrom
                    |> Maybe.map (.sourceNodeId >> (==) nodeId)
                    |> Maybe.withDefault False
            )


isForkTarget : UUID -> Project -> Bool
isForkTarget nodeId project =
    project.columns
        |> List.any (isForkEntryNode nodeId)


isJoinSource : UUID -> Column -> Bool
isJoinSource nodeId column =
    case column.joinedInto of
        Just _ ->
            column
                |> lastNode
                |> Maybe.map (.id >> (==) nodeId)
                |> Maybe.withDefault False

        Nothing ->
            False


isJoinSourceInProject : UUID -> Project -> Bool
isJoinSourceInProject nodeId project =
    project.columns
        |> List.any (isJoinSource nodeId)


isJoinTarget : UUID -> Project -> Bool
isJoinTarget nodeId project =
    project.columns
        |> List.any
            (\column ->
                column.joinedInto
                    |> Maybe.map (.targetNodeId >> (==) nodeId)
                    |> Maybe.withDefault False
            )


locateNode : UUID -> Project -> Maybe ( Column, Node )
locateNode nodeId project =
    project.columns
        |> List.filterMap
            (\column ->
                findNodeInColumn nodeId column
                    |> Maybe.map (\node -> ( column, node ))
            )
        |> List.head


findColumn : UUID -> List Column -> Maybe Column
findColumn columnId columns =
    columns
        |> List.filter (.id >> (==) columnId)
        |> List.head


findNodeInColumn : UUID -> Column -> Maybe Node
findNodeInColumn nodeId column =
    column.nodes
        |> List.filter (.id >> (==) nodeId)
        |> List.head


nodeIndex : UUID -> List Node -> Maybe Int
nodeIndex nodeId nodes =
    nodes
        |> List.indexedMap Tuple.pair
        |> List.filter (\( _, node ) -> node.id == nodeId)
        |> List.head
        |> Maybe.map Tuple.first


nodeRow : Column -> UUID -> Maybe Int
nodeRow column nodeId =
    nodeIndex nodeId column.nodes
        |> Maybe.map (\index -> column.baseRow + index)


lastNode : Column -> Maybe Node
lastNode column =
    column.nodes
        |> List.reverse
        |> List.head


sortColumns : List Column -> List Column
sortColumns columns =
    List.sortBy .order columns
