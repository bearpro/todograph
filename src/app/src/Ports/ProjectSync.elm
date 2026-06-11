port module Ports.ProjectSync exposing
    ( checkServerProjects
    , debounceSaveServerProject
    , fetchServerProject
    , fetchServerProjectVersion
    , saveServerProject
    , serverAvailabilityChanged
    , serverProjectLoaded
    , serverProjectSaveAccepted
    , serverProjectSaveRejected
    , serverProjectUpdated
    , serverProjectVersionLoaded
    , serverProjectsChecked
    , subscribeProject
    , syncFailed
    , unsubscribeProject
    )

import Json.Decode as Decode
import Json.Encode as Encode


port checkServerProjects : Encode.Value -> Cmd msg


port fetchServerProject : String -> Cmd msg


port fetchServerProjectVersion : String -> Cmd msg


port saveServerProject : Encode.Value -> Cmd msg


port debounceSaveServerProject : Encode.Value -> Cmd msg


port subscribeProject : String -> Cmd msg


port unsubscribeProject : String -> Cmd msg


port serverProjectsChecked : (Decode.Value -> msg) -> Sub msg


port serverAvailabilityChanged : (Bool -> msg) -> Sub msg


port serverProjectVersionLoaded : (Decode.Value -> msg) -> Sub msg


port serverProjectLoaded : (Decode.Value -> msg) -> Sub msg


port serverProjectSaveAccepted : (Decode.Value -> msg) -> Sub msg


port serverProjectSaveRejected : (Decode.Value -> msg) -> Sub msg


port serverProjectUpdated : (Decode.Value -> msg) -> Sub msg


port syncFailed : (Decode.Value -> msg) -> Sub msg
