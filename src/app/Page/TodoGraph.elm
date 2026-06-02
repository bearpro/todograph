module Page.TodoGraph exposing (..)

import Browser exposing (Document)
import Html exposing (text)
import UUID exposing (UUID)


type alias Model =
    { id : UUID
    , name : String
    }


type alias Msg =
    ()


init : Model -> ( Model, Cmd Msg )
init model =
    ( model, Cmd.none )


view : Model -> Document msg
view model =
    { title = model.name
    , body =
        [ text ("Project " ++ model.name) ]
    }
