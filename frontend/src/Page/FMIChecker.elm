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
    div
        []
        [ h1 [ class "display-4, text-center" ] [ text "FMI Checker" ]
        , div
            [ class "form-group" ]
            [ label [ for "fileSelector" ] [ text "Model Description or FMU" ]
            , input
                [ id "fileSelector"
                , type_ "file"
                , multiple False
                , accept "text/xml, application/xml, application/zip"
                , on "change" (D.map FileParameter Utilities.filesDecoder)
                , class "form-control-file"
                ]
                []
            ]
        , button [ class "btn btn-primary", onClick Submit, disabled (model.file == Nothing) ] [ text "Check ModelDescription" ]
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
            case model.file of
                Nothing ->
                    ( model, Cmd.none )

                Just f_ ->
                    ( { model | checkResult = Just "Running checker..." }
                    , Http.post
                        { url = ApiReq.correctURL model.basePath "api/fmichecker"
                        , body = Http.multipartBody [ Http.filePart (File.name f_) f_ ]
                        , expect = Http.expectString SubmitResponse
                        }
                    )
