module Domain.Project exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode
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
    { sourceChainId : UUID
    , sourceNodeId : UUID
    }


type alias JoinRef =
    { targetChainId : UUID
    , targetNodeId : UUID
    }


type alias Chain =
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
    , updatedAt : Time.Posix
    , sync : Bool
    , syncPending : Bool
    , chains : List Chain
    }


schemaVersion : Int
schemaVersion =
    3


epoch : Time.Posix
epoch =
    Time.millisToPosix 0


updatedAtEncoder : Time.Posix -> Encode.Value
updatedAtEncoder updatedAt =
    Encode.int (Time.posixToMillis updatedAt)


updatedAtDecoder : Decode.Decoder Time.Posix
updatedAtDecoder =
    Decode.int
        |> Decode.map Time.millisToPosix


maxPosix : Time.Posix -> Time.Posix -> Time.Posix
maxPosix left right =
    Time.millisToPosix
        (max (Time.posixToMillis left) (Time.posixToMillis right))


touch : Time.Posix -> Project -> Project
touch updatedAt project =
    { project | updatedAt = maxPosix project.updatedAt updatedAt }


setSync : Bool -> Project -> Project
setSync sync project =
    { project
        | sync = sync
        , syncPending =
            if sync then
                project.syncPending

            else
                False
    }


setSyncPending : Bool -> Project -> Project
setSyncPending syncPending project =
    { project | syncPending = syncPending }


uuidEncoder : UUID -> Encode.Value
uuidEncoder uuid =
    Encode.string (UUID.toString uuid)


uuidDecoder : Decode.Decoder UUID
uuidDecoder =
    Decode.string
        |> Decode.andThen
            (\value ->
                case UUID.fromString value of
                    Ok uuid ->
                        Decode.succeed uuid

                    Err _ ->
                        Decode.fail ("Invalid UUID: " ++ value)
            )


maybeEncoder : (value -> Encode.Value) -> Maybe value -> Encode.Value
maybeEncoder encoder maybeValue =
    case maybeValue of
        Just value ->
            encoder value

        Nothing ->
            Encode.null


timerEncoder : Timer -> Encode.Value
timerEncoder timer =
    case timer of
        Started startedAt ->
            Encode.object
                [ ( "state", Encode.string "started" )
                , ( "startedAt", Encode.int (Time.posixToMillis startedAt) )
                ]

        Stopped seconds ->
            Encode.object
                [ ( "state", Encode.string "stopped" )
                , ( "seconds", Encode.int seconds )
                ]


timerDecoder : Decode.Decoder Timer
timerDecoder =
    Decode.field "state" Decode.string
        |> Decode.andThen
            (\state ->
                case state of
                    "started" ->
                        Decode.field "startedAt" Decode.int
                            |> Decode.map (Time.millisToPosix >> Started)

                    "stopped" ->
                        Decode.field "seconds" Decode.int
                            |> Decode.map Stopped

                    _ ->
                        Decode.fail ("Invalid timer state: " ++ state)
            )


nodeEncoder : Node -> Encode.Value
nodeEncoder node =
    Encode.object
        [ ( "id", uuidEncoder node.id )
        , ( "text", Encode.string node.text )
        , ( "status", Encode.bool node.status )
        , ( "timer", maybeEncoder timerEncoder node.timer )
        , ( "description", maybeEncoder Encode.string node.description )
        ]


nodeDecoder : Decode.Decoder Node
nodeDecoder =
    Decode.map5 Node
        (Decode.field "id" uuidDecoder)
        (Decode.field "text" Decode.string)
        (Decode.field "status" Decode.bool)
        (Decode.field "timer" (Decode.nullable timerDecoder))
        (Decode.field "description" (Decode.nullable Decode.string))


forkRefEncoder : ForkRef -> Encode.Value
forkRefEncoder ref =
    Encode.object
        [ ( "sourceChainId", uuidEncoder ref.sourceChainId )
        , ( "sourceNodeId", uuidEncoder ref.sourceNodeId )
        ]


forkRefDecoder : Decode.Decoder ForkRef
forkRefDecoder =
    Decode.map2 ForkRef
        (Decode.field "sourceChainId" uuidDecoder)
        (Decode.field "sourceNodeId" uuidDecoder)


joinRefEncoder : JoinRef -> Encode.Value
joinRefEncoder ref =
    Encode.object
        [ ( "targetChainId", uuidEncoder ref.targetChainId )
        , ( "targetNodeId", uuidEncoder ref.targetNodeId )
        ]


joinRefDecoder : Decode.Decoder JoinRef
joinRefDecoder =
    Decode.map2 JoinRef
        (Decode.field "targetChainId" uuidDecoder)
        (Decode.field "targetNodeId" uuidDecoder)


chainEncoder : Chain -> Encode.Value
chainEncoder chain =
    Encode.object
        [ ( "id", uuidEncoder chain.id )
        , ( "order", Encode.int chain.order )
        , ( "baseRow", Encode.int chain.baseRow )
        , ( "name", maybeEncoder Encode.string chain.name )
        , ( "nodes", Encode.list nodeEncoder chain.nodes )
        , ( "forkedFrom", maybeEncoder forkRefEncoder chain.forkedFrom )
        , ( "joinedInto", maybeEncoder joinRefEncoder chain.joinedInto )
        ]


chainDecoder : Decode.Decoder Chain
chainDecoder =
    Decode.map7 Chain
        (Decode.field "id" uuidDecoder)
        (Decode.field "order" Decode.int)
        (Decode.field "baseRow" Decode.int)
        (Decode.field "name" (Decode.nullable Decode.string))
        (Decode.field "nodes" (Decode.list nodeDecoder))
        (Decode.field "forkedFrom" (Decode.nullable forkRefDecoder))
        (Decode.field "joinedInto" (Decode.nullable joinRefDecoder))


projectEncoder : Project -> Encode.Value
projectEncoder project =
    Encode.object
        [ ( "schemaVersion", Encode.int schemaVersion )
        , ( "id", uuidEncoder project.id )
        , ( "name", maybeEncoder Encode.string project.name )
        , ( "updatedAt", updatedAtEncoder project.updatedAt )
        , ( "sync", Encode.bool project.sync )
        , ( "syncPending", Encode.bool project.syncPending )
        , ( "chains", Encode.list chainEncoder project.chains )
        ]


projectPayloadEncoder : Project -> Encode.Value
projectPayloadEncoder project =
    Encode.object
        [ ( "schemaVersion", Encode.int schemaVersion )
        , ( "id", uuidEncoder project.id )
        , ( "name", maybeEncoder Encode.string project.name )
        , ( "updatedAt", updatedAtEncoder project.updatedAt )
        , ( "chains", Encode.list chainEncoder project.chains )
        ]


projectDecoder : Decode.Decoder Project
projectDecoder =
    Decode.field "schemaVersion" Decode.int
        |> Decode.andThen
            (\version ->
                if version == schemaVersion then
                    Decode.map6 Project
                        (Decode.field "id" uuidDecoder)
                        (Decode.field "name" (Decode.nullable Decode.string))
                        (Decode.field "updatedAt" updatedAtDecoder)
                        (Decode.oneOf
                            [ Decode.field "sync" Decode.bool
                            , Decode.succeed False
                            ]
                        )
                        (Decode.oneOf
                            [ Decode.field "syncPending" Decode.bool
                            , Decode.succeed False
                            ]
                        )
                        (Decode.field "chains" (Decode.list chainDecoder))

                else
                    Decode.fail ("Unsupported project schema version: " ++ String.fromInt version)
            )


projectsDecoder : Decode.Decoder (List Project)
projectsDecoder =
    Decode.list
        (Decode.oneOf
            [ Decode.map Just projectDecoder
            , Decode.succeed Nothing
            ]
        )
        |> Decode.map (List.filterMap identity)


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
    , updatedAt = epoch
    , sync = False
    , syncPending = False
    , chains =
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
appendNode chainId node project =
    { project
        | chains =
            project.chains
                |> List.map
                    (\chain ->
                        if chain.id == chainId then
                            { chain | nodes = chain.nodes ++ [ node ] }

                        else
                            chain
                    )
    }


insertNodeAfter : UUID -> Node -> Project -> Project
insertNodeAfter afterNodeId newNode project =
    { project
        | chains =
            project.chains
                |> List.map
                    (\chain ->
                        { chain
                            | nodes =
                                chain.nodes
                                    |> insertNodeAfterInList afterNodeId newNode
                        }
                    )
    }
        |> realignForkChainBaseRows


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
        | chains =
            project.chains
                |> List.map
                    (\chain ->
                        { chain
                            | nodes =
                                chain.nodes
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
        | chains =
            project.chains
                |> List.map
                    (\chain ->
                        { chain
                            | nodes =
                                chain.nodes
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


forkChain : UUID -> UUID -> UUID -> Node -> Project -> Project
forkChain sourceChainId sourceNodeId newChainId firstNode project =
    let
        sourceChain =
            findChain sourceChainId project.chains

        sourceOrder =
            sourceChain
                |> Maybe.map .order
                |> Maybe.withDefault 0

        sourceRow =
            sourceChain
                |> Maybe.map
                    (\chain ->
                        chain.baseRow
                            + Maybe.withDefault 0 (nodeIndex sourceNodeId chain.nodes)
                    )
                |> Maybe.withDefault 0

        shiftedChains =
            project.chains
                |> List.map
                    (\chain ->
                        if chain.order > sourceOrder then
                            { chain | order = chain.order + 1 }

                        else
                            chain
                    )

        newChain =
            { id = newChainId
            , order = sourceOrder + 1
            , baseRow = sourceRow
            , name = Nothing
            , nodes = [ firstNode ]
            , forkedFrom =
                Just
                    { sourceChainId = sourceChainId
                    , sourceNodeId = sourceNodeId
                    }
            , joinedInto = Nothing
            }
    in
    { project | chains = sortChains (newChain :: shiftedChains) }


joinChainIntoPrevious : UUID -> Project -> Project
joinChainIntoPrevious chainId project =
    case findChain chainId project.chains of
        Just chain ->
            project.chains
                |> List.filter (\candidate -> candidate.order < chain.order)
                |> sortChains
                |> List.reverse
                |> List.head
                |> Maybe.andThen
                    (\targetChain ->
                        lastNode targetChain
                            |> Maybe.map (\targetNode -> ( targetChain, targetNode ))
                    )
                |> Maybe.map
                    (\( targetChain, targetNode ) ->
                        { project
                            | chains =
                                project.chains
                                    |> List.map
                                        (\candidate ->
                                            if candidate.id == chainId then
                                                { candidate
                                                    | joinedInto =
                                                        Just
                                                            { targetChainId = targetChain.id
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


joinChainToNode : UUID -> UUID -> Project -> Project
joinChainToNode sourceChainId targetNodeId project =
    if canJoinChainToNode sourceChainId targetNodeId project then
        case locateNode targetNodeId project of
            Just ( targetChain, targetNode ) ->
                { project
                    | chains =
                        project.chains
                            |> List.map
                                (\chain ->
                                    if chain.id == sourceChainId then
                                        { chain
                                            | joinedInto =
                                                Just
                                                    { targetChainId = targetChain.id
                                                    , targetNodeId = targetNode.id
                                                    }
                                        }

                                    else
                                        chain
                                )
                }

            Nothing ->
                project

    else
        project


unjoinChain : UUID -> Project -> Project
unjoinChain chainId project =
    { project
        | chains =
            project.chains
                |> List.map
                    (\chain ->
                        if chain.id == chainId then
                            { chain | joinedInto = Nothing }

                        else
                            chain
                    )
    }


canJoinChainToNode : UUID -> UUID -> Project -> Bool
canJoinChainToNode sourceChainId targetNodeId project =
    case ( findChain sourceChainId project.chains, locateNode targetNodeId project ) of
        ( Just sourceChain, Just ( targetChain, _ ) ) ->
            case lastNode sourceChain of
                Just sourceNode ->
                    sourceChain.id
                        /= targetChain.id
                        && sourceChain.joinedInto
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
    project.chains
        |> List.concatMap chainDirectedEdges


chainDirectedEdges : Chain -> List ( UUID, UUID )
chainDirectedEdges chain =
    verticalDirectedEdges chain.nodes
        ++ forkDirectedEdge chain
        ++ joinDirectedEdge chain


verticalDirectedEdges : List Node -> List ( UUID, UUID )
verticalDirectedEdges nodes =
    case nodes of
        source :: target :: rest ->
            ( source.id, target.id ) :: verticalDirectedEdges (target :: rest)

        _ ->
            []


forkDirectedEdge : Chain -> List ( UUID, UUID )
forkDirectedEdge chain =
    case ( chain.forkedFrom, chain.nodes ) of
        ( Just forkRef, firstNode :: _ ) ->
            [ ( forkRef.sourceNodeId, firstNode.id ) ]

        _ ->
            []


joinDirectedEdge : Chain -> List ( UUID, UUID )
joinDirectedEdge chain =
    case ( chain.joinedInto, lastNode chain ) of
        ( Just joinRef, Just sourceNode ) ->
            [ ( sourceNode.id, joinRef.targetNodeId ) ]

        _ ->
            []


canDeleteNode : UUID -> Project -> Bool
canDeleteNode nodeId project =
    case locateNode nodeId project of
        Just ( chain, _ ) ->
            if canDeleteForkEntryNode nodeId chain project then
                True

            else if isJoinSource nodeId chain then
                not (isForkSource nodeId project)
                    && not (isForkEntryNode nodeId chain)
                    && not (isJoinTarget nodeId project)

            else
                not (hasInterChainEdge nodeId project)

        Nothing ->
            False


deleteNode : UUID -> Project -> Project
deleteNode nodeId project =
    case locateNode nodeId project of
        Just ( chain, _ ) ->
            if canDeleteForkEntryNode nodeId chain project then
                deleteChain chain.id project

            else if isJoinSource nodeId chain then
                if canDeleteNode nodeId project then
                    deleteNodeFromChain True chain.id nodeId project

                else
                    project

            else if canDeleteNode nodeId project then
                deleteNodeFromChain False chain.id nodeId project

            else
                project

        Nothing ->
            project


deleteNodeFromChain : Bool -> UUID -> UUID -> Project -> Project
deleteNodeFromChain clearJoin chainId nodeId project =
    { project
        | chains =
            project.chains
                |> List.map
                    (\chain ->
                        if chain.id == chainId then
                            { chain
                                | nodes =
                                    chain.nodes
                                        |> List.filter (.id >> (/=) nodeId)
                                , joinedInto =
                                    if clearJoin then
                                        Nothing

                                    else
                                        chain.joinedInto
                            }

                        else
                            chain
                    )
    }
        |> realignForkChainBaseRows


deleteChain : UUID -> Project -> Project
deleteChain chainId project =
    { project
        | chains =
            project.chains
                |> List.filter (.id >> (/=) chainId)
                |> normalizeChainOrders
    }
        |> realignForkChainBaseRows


realignForkChainBaseRows : Project -> Project
realignForkChainBaseRows project =
    let
        alignChains remainingChains alignedChains =
            case remainingChains of
                chain :: rest ->
                    let
                        alignedChain =
                            case chain.forkedFrom of
                                Just forkRef ->
                                    alignedChains
                                        |> findChain forkRef.sourceChainId
                                        |> Maybe.andThen (\sourceChain -> nodeRow sourceChain forkRef.sourceNodeId)
                                        |> Maybe.map (\sourceRow -> { chain | baseRow = sourceRow })
                                        |> Maybe.withDefault chain

                                Nothing ->
                                    chain
                    in
                    alignChains rest (alignedChain :: alignedChains)

                [] ->
                    alignedChains
                        |> List.reverse
    in
    { project | chains = alignChains (sortChains project.chains) [] }


normalizeChainOrders : List Chain -> List Chain
normalizeChainOrders chains =
    chains
        |> sortChains
        |> List.indexedMap (\index chain -> { chain | order = index })


hasInterChainEdge : UUID -> Project -> Bool
hasInterChainEdge nodeId project =
    isForkSource nodeId project
        || isForkTarget nodeId project
        || isJoinSourceInProject nodeId project
        || isJoinTarget nodeId project


canDeleteForkEntryNode : UUID -> Chain -> Project -> Bool
canDeleteForkEntryNode nodeId chain project =
    isForkEntryNode nodeId chain
        && (List.length chain.nodes == 1)
        && not (isForkSource nodeId project)
        && not (isJoinTarget nodeId project)


isForkEntryNode : UUID -> Chain -> Bool
isForkEntryNode nodeId chain =
    case ( chain.forkedFrom, chain.nodes ) of
        ( Just _, firstNode :: _ ) ->
            firstNode.id == nodeId

        _ ->
            False


isForkSource : UUID -> Project -> Bool
isForkSource nodeId project =
    project.chains
        |> List.any
            (\chain ->
                chain.forkedFrom
                    |> Maybe.map (.sourceNodeId >> (==) nodeId)
                    |> Maybe.withDefault False
            )


isForkTarget : UUID -> Project -> Bool
isForkTarget nodeId project =
    project.chains
        |> List.any (isForkEntryNode nodeId)


isJoinSource : UUID -> Chain -> Bool
isJoinSource nodeId chain =
    case chain.joinedInto of
        Just _ ->
            chain
                |> lastNode
                |> Maybe.map (.id >> (==) nodeId)
                |> Maybe.withDefault False

        Nothing ->
            False


isJoinSourceInProject : UUID -> Project -> Bool
isJoinSourceInProject nodeId project =
    project.chains
        |> List.any (isJoinSource nodeId)


isJoinTarget : UUID -> Project -> Bool
isJoinTarget nodeId project =
    project.chains
        |> List.any
            (\chain ->
                chain.joinedInto
                    |> Maybe.map (.targetNodeId >> (==) nodeId)
                    |> Maybe.withDefault False
            )


locateNode : UUID -> Project -> Maybe ( Chain, Node )
locateNode nodeId project =
    project.chains
        |> List.filterMap
            (\chain ->
                findNodeInChain nodeId chain
                    |> Maybe.map (\node -> ( chain, node ))
            )
        |> List.head


findChain : UUID -> List Chain -> Maybe Chain
findChain chainId chains =
    chains
        |> List.filter (.id >> (==) chainId)
        |> List.head


findNodeInChain : UUID -> Chain -> Maybe Node
findNodeInChain nodeId chain =
    chain.nodes
        |> List.filter (.id >> (==) nodeId)
        |> List.head


nodeIndex : UUID -> List Node -> Maybe Int
nodeIndex nodeId nodes =
    nodes
        |> List.indexedMap Tuple.pair
        |> List.filter (\( _, node ) -> node.id == nodeId)
        |> List.head
        |> Maybe.map Tuple.first


nodeRow : Chain -> UUID -> Maybe Int
nodeRow chain nodeId =
    nodeIndex nodeId chain.nodes
        |> Maybe.map (\index -> chain.baseRow + index)


lastNode : Chain -> Maybe Node
lastNode chain =
    chain.nodes
        |> List.reverse
        |> List.head


sortChains : List Chain -> List Chain
sortChains chains =
    List.sortBy (\chain -> ( chain.order, UUID.toString chain.id )) chains
