module Control.FluentIcon exposing (Icon(..), view)

import Html exposing (Html, span)
import Html.Attributes exposing (attribute, class, style)


type Icon
    = AddCircle
    | AddSquare
    | ArrowClockwise
    | CloudCheckmark
    | CloudOff
    | CloudSync
    | Copy
    | Delete
    | Edit
    | Link
    | LinkDismiss
    | Play
    | Stop


view : Icon -> Html msg
view icon =
    span
        [ class "fluent-icon"
        , attribute "aria-hidden" "true"
        , style "-webkit-mask-image" ("url(\"" ++ path icon ++ "\")")
        , style "mask-image" ("url(\"" ++ path icon ++ "\")")
        ]
        []


path : Icon -> String
path icon =
    "/icons/fluent/"
        ++ (case icon of
                AddCircle ->
                    "add_circle_20_regular.svg"

                AddSquare ->
                    "add_square_20_regular.svg"

                ArrowClockwise ->
                    "arrow_clockwise_20_regular.svg"

                CloudCheckmark ->
                    "cloud_checkmark_20_regular.svg"

                CloudOff ->
                    "cloud_off_20_regular.svg"

                CloudSync ->
                    "cloud_sync_20_regular.svg"

                Copy ->
                    "copy_20_regular.svg"

                Delete ->
                    "delete_20_regular.svg"

                Edit ->
                    "edit_20_regular.svg"

                Link ->
                    "link_20_regular.svg"

                LinkDismiss ->
                    "link_dismiss_20_regular.svg"

                Play ->
                    "play_20_regular.svg"

                Stop ->
                    "stop_20_regular.svg"
           )
