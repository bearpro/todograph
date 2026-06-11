module Domain.ProjectTest exposing (suite)

import Domain.Project as Project
import Expect
import Json.Decode as Decode
import Json.Encode as Encode
import Test exposing (Test, describe, test)
import UUID exposing (UUID)


suite : Test
suite =
    describe "Project JSON"
        [ test "defaults syncPending to false for older stored projects" <|
            \_ ->
                Decode.decodeString Project.projectDecoder oldStoredProjectJson
                    |> Result.map .syncPending
                    |> Expect.equal (Ok False)
        , test "preserves syncPending in local storage encoding" <|
            \_ ->
                Project.projectEncoder pendingProject
                    |> Decode.decodeValue (Decode.field "syncPending" Decode.bool)
                    |> Expect.equal (Ok True)
        , test "excludes syncPending from server payload encoding" <|
            \_ ->
                Project.projectPayloadEncoder pendingProject
                    |> Decode.decodeValue (Decode.field "syncPending" Decode.bool)
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        ]


pendingProject : Project.Project
pendingProject =
    Project.initialProject projectId nodeId
        |> Project.setSync True
        |> Project.setSyncPending True


oldStoredProjectJson : String
oldStoredProjectJson =
    Encode.encode 0
        (Encode.object
            [ ( "schemaVersion", Encode.int Project.schemaVersion )
            , ( "id", Project.uuidEncoder projectId )
            , ( "name", Encode.null )
            , ( "updatedAt", Encode.int 0 )
            , ( "sync", Encode.bool True )
            , ( "chains", Encode.list Project.chainEncoder pendingProject.chains )
            ]
        )


projectId : UUID
projectId =
    uuid "00000000-0000-0000-0000-000000000001"


nodeId : UUID
nodeId =
    uuid "00000000-0000-0000-0000-000000000002"


uuid : String -> UUID
uuid value =
    case UUID.fromString value of
        Ok parsed ->
            parsed

        Err _ ->
            UUID.forName value UUID.dnsNamespace
