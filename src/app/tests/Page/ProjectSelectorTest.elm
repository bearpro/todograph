module Page.ProjectSelectorTest exposing (suite)

import Domain.Project as Project
import Expect
import Page.ProjectSelector as ProjectSelector
import Test exposing (Test, describe, test)
import UUID exposing (UUID)


suite : Test
suite =
    describe "project sync display"
        [ test "shows off when sync is disabled" <|
            \_ ->
                ProjectSelector.projectSyncDisplay baseProject
                    |> Expect.equal ProjectSelector.SyncOff
        , test "shows clean when sync is enabled and no save is pending" <|
            \_ ->
                baseProject
                    |> Project.setSync True
                    |> ProjectSelector.projectSyncDisplay
                    |> Expect.equal ProjectSelector.SyncClean
        , test "shows pending when a synced project is waiting for save acknowledgement" <|
            \_ ->
                baseProject
                    |> Project.setSync True
                    |> Project.setSyncPending True
                    |> ProjectSelector.projectSyncDisplay
                    |> Expect.equal ProjectSelector.SyncPending
        ]


baseProject : Project.Project
baseProject =
    Project.initialProject
        (uuid "00000000-0000-0000-0000-000000000001")
        (uuid "00000000-0000-0000-0000-000000000002")


uuid : String -> UUID
uuid value =
    case UUID.fromString value of
        Ok parsed ->
            parsed

        Err _ ->
            UUID.forName value UUID.dnsNamespace
