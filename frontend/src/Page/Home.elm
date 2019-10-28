module Page.Home exposing (Model, Msg, init, update, view)

import Html exposing (..)


type alias Model =
    ()


type alias Msg =
    ()


init : ( Model, Cmd Msg )
init =
    ( (), Cmd.none )


view : Model -> Html Msg
view _ =
    text "This website is the frontend for various INTO-CPS Association related utilities"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )
