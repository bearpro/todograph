module Page.TodoGraph.Layout exposing
    ( ChainLayout
    , chainEndRow
    , chainStartRow
    , layoutChains
    , maxPhysicalColumn
    )

import Domain.Project as Project
import UUID exposing (UUID)


type alias ChainLayout =
    { chain : Project.Chain
    , physicalColumn : Int
    }


layoutChains : List Project.Chain -> List ChainLayout
layoutChains chains =
    let
        sortedChains =
            Project.sortChains chains
    in
    placeChains sortedChains [] sortedChains


chainStartRow : Project.Chain -> Int
chainStartRow chain =
    chain.baseRow


chainEndRow : Project.Chain -> Int
chainEndRow chain =
    chain.baseRow + max 0 (List.length chain.nodes - 1)


maxPhysicalColumn : List ChainLayout -> Int
maxPhysicalColumn layouts =
    layouts
        |> List.map .physicalColumn
        |> List.maximum
        |> Maybe.withDefault 0


placeChains : List Project.Chain -> List ChainLayout -> List Project.Chain -> List ChainLayout
placeChains allChains placed remaining =
    case takeNextPlaceableChain allChains placed remaining of
        Just ( chain, rest ) ->
            let
                nextLayout =
                    { chain = chain
                    , physicalColumn =
                        leftmostAvailableColumn
                            (minimumPhysicalColumn placed chain)
                            chain
                            placed
                    }
            in
            placeChains allChains (placed ++ [ nextLayout ]) rest

        Nothing ->
            case remaining of
                chain :: rest ->
                    let
                        nextLayout =
                            { chain = chain
                            , physicalColumn =
                                leftmostAvailableColumn
                                    (minimumPhysicalColumn placed chain)
                                    chain
                                    placed
                            }
                    in
                    placeChains allChains (placed ++ [ nextLayout ]) rest

                [] ->
                    placed


takeNextPlaceableChain : List Project.Chain -> List ChainLayout -> List Project.Chain -> Maybe ( Project.Chain, List Project.Chain )
takeNextPlaceableChain allChains placed remaining =
    takeNextPlaceableChainHelp allChains placed [] remaining


takeNextPlaceableChainHelp : List Project.Chain -> List ChainLayout -> List Project.Chain -> List Project.Chain -> Maybe ( Project.Chain, List Project.Chain )
takeNextPlaceableChainHelp allChains placed skipped remaining =
    case remaining of
        chain :: rest ->
            if sourceIsReady allChains placed chain then
                Just ( chain, List.reverse skipped ++ rest )

            else
                takeNextPlaceableChainHelp allChains placed (chain :: skipped) rest

        [] ->
            Nothing


sourceIsReady : List Project.Chain -> List ChainLayout -> Project.Chain -> Bool
sourceIsReady allChains placed chain =
    case chain.forkedFrom of
        Just forkRef ->
            not (chainExists forkRef.sourceChainId allChains)
                || (placed
                        |> findChainLayout forkRef.sourceChainId
                        |> Maybe.map (\_ -> True)
                        |> Maybe.withDefault False
                   )

        Nothing ->
            True


chainExists : UUID -> List Project.Chain -> Bool
chainExists chainId chains =
    chains
        |> List.any (.id >> (==) chainId)


minimumPhysicalColumn : List ChainLayout -> Project.Chain -> Int
minimumPhysicalColumn placed chain =
    case chain.forkedFrom of
        Just forkRef ->
            placed
                |> findChainLayout forkRef.sourceChainId
                |> Maybe.map (\layout -> layout.physicalColumn + 1)
                |> Maybe.withDefault 0

        Nothing ->
            0


leftmostAvailableColumn : Int -> Project.Chain -> List ChainLayout -> Int
leftmostAvailableColumn candidate chain placed =
    if columnHasSpace candidate chain placed then
        candidate

    else
        leftmostAvailableColumn (candidate + 1) chain placed


columnHasSpace : Int -> Project.Chain -> List ChainLayout -> Bool
columnHasSpace candidate chain placed =
    placed
        |> List.filter (.physicalColumn >> (==) candidate)
        |> List.all (\layout -> not (rowIntervalsOverlap chain layout.chain))


rowIntervalsOverlap : Project.Chain -> Project.Chain -> Bool
rowIntervalsOverlap left right =
    not
        ((chainEndRow left < chainStartRow right)
            || (chainEndRow right < chainStartRow left)
        )


findChainLayout : UUID -> List ChainLayout -> Maybe ChainLayout
findChainLayout chainId layouts =
    layouts
        |> List.filter (.chain >> .id >> (==) chainId)
        |> List.head
