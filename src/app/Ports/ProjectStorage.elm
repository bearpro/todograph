port module Ports.ProjectStorage exposing
    ( deleteProject
    , loadProjects
    , projectsLoaded
    , saveProject
    , storageFailed
    )

import Json.Decode as Decode
import Json.Encode as Encode


port loadProjects : () -> Cmd msg


port saveProject : Encode.Value -> Cmd msg


port deleteProject : String -> Cmd msg


port projectsLoaded : (Decode.Value -> msg) -> Sub msg


port storageFailed : (String -> msg) -> Sub msg
