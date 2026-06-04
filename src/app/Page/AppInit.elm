module Page.AppInit exposing (..)

import Browser exposing (Document)
import Html exposing (text)


type alias Model =
    ()


init : Model
init =
    ()


view : Model -> Document msg
view _ =
    { title = "loading.."
    , body =
        [ text "loading.." ]
    }
