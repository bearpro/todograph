module Page.TodoGraph.LayoutTest exposing (suite)

import Domain.Project as Project
import Expect
import Page.TodoGraph.Layout as Layout
import Test exposing (Test, describe, test)
import UUID exposing (UUID)


suite : Test
suite =
    describe "TodoGraph chain layout"
        [ test "packs non-overlapping fork chains into the same physical column" <|
            \_ ->
                let
                    base =
                        chain "00000000-0000-0000-0000-000000000001" 0 0 1 Nothing

                    forkAtBase =
                        chain "00000000-0000-0000-0000-000000000002" 1 0 1 (Just (forkFrom base))

                    forkAboveBase =
                        chain "00000000-0000-0000-0000-000000000003" 2 1 1 (Just (forkFrom base))
                in
                [ base, forkAtBase, forkAboveBase ]
                    |> Layout.layoutChains
                    |> physicalColumns
                    |> Expect.equal
                        [ ( base.id, 0 )
                        , ( forkAtBase.id, 1 )
                        , ( forkAboveBase.id, 1 )
                        ]
        , test "moves a fork chain right when logical rows overlap" <|
            \_ ->
                let
                    base =
                        chain "00000000-0000-0000-0000-000000000001" 0 0 1 Nothing

                    longFork =
                        chain "00000000-0000-0000-0000-000000000002" 1 0 2 (Just (forkFrom base))

                    overlappingFork =
                        chain "00000000-0000-0000-0000-000000000003" 2 1 1 (Just (forkFrom base))
                in
                [ base, longFork, overlappingFork ]
                    |> Layout.layoutChains
                    |> physicalColumns
                    |> Expect.equal
                        [ ( base.id, 0 )
                        , ( longFork.id, 1 )
                        , ( overlappingFork.id, 2 )
                        ]
        , test "never places a fork chain in or left of its source physical column" <|
            \_ ->
                let
                    root =
                        chain "00000000-0000-0000-0000-000000000001" 0 0 1 Nothing

                    source =
                        chain "00000000-0000-0000-0000-000000000002" 1 1 1 (Just (forkFrom root))

                    child =
                        chain "00000000-0000-0000-0000-000000000003" 2 1 1 (Just (forkFrom source))
                in
                [ root, source, child ]
                    |> Layout.layoutChains
                    |> physicalColumns
                    |> Expect.equal
                        [ ( root.id, 0 )
                        , ( source.id, 1 )
                        , ( child.id, 2 )
                        ]
        , test "uses UUID as a deterministic tie-breaker for equal semantic order" <|
            \_ ->
                let
                    laterId =
                        chain "00000000-0000-0000-0000-000000000010" 0 0 1 Nothing

                    earlierId =
                        chain "00000000-0000-0000-0000-000000000009" 0 0 1 Nothing
                in
                [ laterId, earlierId ]
                    |> Layout.layoutChains
                    |> physicalColumns
                    |> Expect.equal
                        [ ( earlierId.id, 0 )
                        , ( laterId.id, 1 )
                        ]
        , test "defers a fork chain until its source has been placed" <|
            \_ ->
                let
                    source =
                        chain "00000000-0000-0000-0000-000000000010" 0 0 1 Nothing

                    childWithEarlierId =
                        chain "00000000-0000-0000-0000-000000000009" 0 0 1 (Just (forkFrom source))
                in
                [ childWithEarlierId, source ]
                    |> Layout.layoutChains
                    |> physicalColumns
                    |> Expect.equal
                        [ ( source.id, 0 )
                        , ( childWithEarlierId.id, 1 )
                        ]
        ]


physicalColumns : List Layout.ChainLayout -> List ( UUID, Int )
physicalColumns layouts =
    layouts
        |> List.map (\layout -> ( layout.chain.id, layout.physicalColumn ))


chain : String -> Int -> Int -> Int -> Maybe Project.ForkRef -> Project.Chain
chain idString order baseRow nodeCount forkedFrom =
    { id = uuid idString
    , order = order
    , baseRow = baseRow
    , name = Nothing
    , nodes =
        List.range 1 nodeCount
            |> List.map
                (\index ->
                    Project.textNode
                        (uuid ("10000000-0000-0000-0000-" ++ String.padLeft 12 '0' (String.fromInt index)))
                        "item"
                )
    , forkedFrom = forkedFrom
    , joinedInto = Nothing
    }


forkFrom : Project.Chain -> Project.ForkRef
forkFrom source =
    { sourceChainId = source.id
    , sourceNodeId =
        source.nodes
            |> List.head
            |> Maybe.map .id
            |> Maybe.withDefault source.id
    }


uuid : String -> UUID
uuid value =
    case UUID.fromString value of
        Ok parsed ->
            parsed

        Err _ ->
            UUID.forName value UUID.dnsNamespace
