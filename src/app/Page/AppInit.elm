module Page.AppInit exposing (..)
import Browser exposing (Document)
import Html exposing (text)
import Task

type alias Model = ()
type Msg = Loaded

loadStub : () -> Msg
loadStub () = Loaded

init : (Model, Cmd Msg)
init = 
    let model = () in
    let cmd = Task.perform (\_ -> Loaded) (Task.succeed ()) in

    (model, cmd)

view : Model -> Document msg
view _ =
    { title = "loading.."
    , body = 
        [ text "loading.." ]
    }