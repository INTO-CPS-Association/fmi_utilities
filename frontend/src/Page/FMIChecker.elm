module Page.FMIChecker exposing (Model, Msg, init, update, view)

import ApiRequests as ApiReq
import File exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode as D
import Utilities


type alias Model =
    { file : Maybe File
    , checkResult : Maybe String
    , basePath : String
    }


type Msg
    = FileParameter (List File)
    | SubmitResponse (Result Http.Error String)
    | Submit


init : String -> ( Model, Cmd Msg )
init basePath =
    ( Model Nothing Nothing basePath, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ label [ for "fileSelector" ] [ text "Model Description" ]
        , input
            [ id "fileSelector"
            , type_ "file"
            , multiple False
            , accept "text/xml"
            , on "change" (D.map FileParameter Utilities.filesDecoder)
            ]
            []
        , div []
            [ text
                (case model.file of
                    Nothing ->
                        ""

                    Just f ->
                        File.name f
                )
            ]
        , button [ onClick Submit, disabled (model.file == Nothing) ] [ text "Check ModelDescription" ]
        , div [ Html.Attributes.style "white-space" "pre-wrap" ]
            -- style is necessary to preserve line breaks
            [ Html.h1 [] [ text "Output" ]
            , text <|
                Maybe.withDefault
                    "No output"
                    model.checkResult
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FileParameter files ->
            ( { model | file = List.head files }, Cmd.none )

        SubmitResponse result ->
            case result of
                Ok res ->
                    ( { model | checkResult = Just res }, Cmd.none )

                Err err ->
                    ( { model | checkResult = (Just << Debug.toString) err }, Cmd.none )

        Submit ->
            ( model
            , case model.file of
                Nothing ->
                    Cmd.none

                Just f_ ->
                    Http.post
                        { url = ApiReq.correctURL model.basePath "api/fmichecker"
                        , body = Http.multipartBody [ Http.filePart (File.name f_) f_ ]
                        , expect = Http.expectString SubmitResponse
                        }
            )
