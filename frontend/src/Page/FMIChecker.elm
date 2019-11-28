module Page.FMIChecker exposing (Model, Msg, init, update, view)

import ApiRequests as ApiReq
import File exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode as D
import Utilities


type FmiVersion
    = FMI2
    | FMI3


type alias Model =
    { file : Maybe File
    , checkResult : Maybe String
    , basePath : String
    , fmiVersion : FmiVersion
    }


type Msg
    = FileParameter (List File)
    | SubmitResponse (Result Http.Error String)
    | Submit
    | RadioClicked FmiVersion


init : String -> ( Model, Cmd Msg )
init basePath =
    ( Model Nothing Nothing basePath FMI2, Cmd.none )


view : Model -> Html Msg
view model =
    div
        [ class "card" ]
        [ div [ class "card-header" ] [ text "FMI Checker" ]
        , div [ class "card-body" ]
            [ div
                [ class "form-group" ]
                [ label [ for "fileSelector" ]
                    [ text "Model Description or FMU:"
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
                ]
            , label [ for "rbGroup" ]
                [ text "FMI Version"
                , div
                    [ Html.Attributes.name "rbGroup" ]
                    [ div [ class "form-check form-check-inline" ]
                        [ input [ class "form-check-input", Html.Attributes.name "rbfmi", type_ "radio", Html.Attributes.id "rbfmi2", checked (model.fmiVersion == FMI2), onClick (RadioClicked FMI2) ]
                            []
                        , label
                            [ class "form-check-label", for "rbfmi2" ]
                            [ text "2.0" ]
                        ]
                    , div [ class "form-check form-check-inline" ]
                        [ input [ class "form-check-input", Html.Attributes.name "rbfmi", type_ "radio", Html.Attributes.id "rbfmi2", checked (model.fmiVersion == FMI3), onClick (RadioClicked FMI3) ]
                            []
                        , label
                            [ class "form-check-label", for "rbfmi3" ]
                            [ text "3.0" ]
                        ]
                    ]
                ]
            , div [] [ button [ onClick Submit, disabled (model.file == Nothing) ] [ text "Run Check" ] ]
            , div [ Html.Attributes.style "white-space" "pre-wrap" ]
                -- style is necessary to preserve line breaks
                [ Html.h1 [] [ text "Output" ]
                , text <|
                    Maybe.withDefault
                        "No output"
                        model.checkResult
                ]
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
                        , body = Http.multipartBody [ Http.filePart (File.name f_) f_, appendFmiVersion model.fmiVersion ]
                        , expect = Http.expectString SubmitResponse
                        }
                    )

        RadioClicked fmiVersion_ ->
            ( { model | fmiVersion = fmiVersion_ }, Cmd.none )


appendFmiVersion : FmiVersion -> Http.Part
appendFmiVersion fmiVersion =
    Http.stringPart "fmiVersion" <|
        case fmiVersion of
            FMI2 ->
                "FMI2"

            FMI3 ->
                "FMI3"
