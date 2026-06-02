module Page.TodoGraph exposing (..)

import Browser exposing (Document)
import Html exposing (text)
import UUID exposing (UUID)

type alias Model = 
    { name: String
    , id: UUID
    }

type alias Msg = ()


init : Model -> Model
init model = model

view : Model -> Document msg
view model =
    { title = model.name
    , body = 
        [ text ("Project " ++ model.name) ]
    }