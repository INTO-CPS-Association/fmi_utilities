module Page.FMUAnalyzer exposing (Model, Msg, init, update, view)

import ApiRequests as ApiReq
import Bytes exposing (Bytes)
import File exposing (..)
import File.Download exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode as D
import Utilities


type alias Model =
    { file : Maybe File
    , checkResult : Maybe (Result String FMUAnalyzerResultJson)
    , nParameter : String
    , lParameter : String
    , resultsFile : Maybe File
    , id : String
    , status : String
    , basePath : String
    }


type Msg
    = NParameter String
    | LParameter String
    | FileParameter (List File)
    | Submit
    | SubmitResponse (Result Http.Error FMUAnalyzerResultJson)
    | GetResultsResponse (Result Http.Error Bytes)
    | DeleteResponse (Result Http.Error String)


init : String -> ( Model, Cmd Msg )
init basePath =
    ( Model Nothing Nothing (String.fromInt 10) (String.fromInt 10) Nothing "" "" basePath, Cmd.none )


textBody : String
textBody =
    """The FMI specification is semi-formal and allows for different interpretations, which lead to different implementations of FMUs.
We recognize that it is  difficult to rigorously specify a standard to the level required by automated synthesis.
Hence, we propose the FMIMOBSTER, which uses model based testing for the evaluation of FMUs.
This has the benefit that the model used to describe the possible behaviors of an FMU can also be used to test it, and the tool can be applied with minimal setup and know-how.

If you disagree with a checking result then we are very interested in hearing about it!
It will allow us to pin down a more accurate interpretation of the standard, and (possibly) correct issues in the tools. Please report issues here: [https://msdl.uantwerpen.be/git/claudio/FMIMOBSTER/issues](https://msdl.uantwerpen.be/git/claudio/FMIMOBSTER/issues)
The parameters and results used in this website are detailed in the documentation: 
[https://msdl.uantwerpen.be/git/claudio/FMIMOBSTER/](https://msdl.uantwerpen.be/git/claudio/FMIMOBSTER/)

To try out the tool do the following:
1. Download the watertank FMU: [https://msdl.uantwerpen.be/git/claudio/FMIMOBSTER/raw/master/src/test/resources/fmus/20-sim/threewatertank1.fmu](https://msdl.uantwerpen.be/git/claudio/FMIMOBSTER/raw/master/src/test/resources/fmus/20-sim/threewatertank1.fmu)
2. Press Analyze FMU

A publication related to this tool has been accepted for publication in American Modelica Conference 2020 under the title: Application of Model-Based Testing to Dynamic Conformance Evaluation of Functional Mockup Units

**Note: The web-version only executes the Linux 64 binaries within the FMUs.**
"""


view : Model -> Html Msg
view model =
    div [ class "card" ]
        [ div [ class "card-header" ] [ text "FMU Analyzer" ]
        , div [ class "card-body" ]
            [ Utilities.markdownToString textBody
            , Html.hr [ style "color" "black", style "margin-left" "0", style "margin-right" "0" ] []
            , label [ for "fileSelector" ]
                [ text "FMU: "
                , input
                    [ id "fileSelector"
                    , type_ "file"
                    , multiple False
                    , accept ".fmu"
                    , on "change" (D.map FileParameter Utilities.filesDecoder)
                    ]
                    []
                ]
            , div []
                [ text
                    (case model.file of
                        Nothing ->
                            ""

                        Just f ->
                            File.name f
                    )
                ]
            , div []
                [ label [ for "nParameter" ]
                    [ text "Parameter n - number of tests (10-100):"
                    , input
                        [ id "nParameter"
                        , type_ "number"
                        , value model.nParameter
                        , onInput NParameter
                        ]
                        []
                    ]
                ]
            , div []
                [ label [ for "lParameter" ]
                    [ text "Parameter l - self-loops (10-100):"
                    , input
                        [ id "lParameter"
                        , type_ "number"
                        , value model.lParameter
                        , onInput LParameter
                        ]
                        []
                    ]
                ]
            , button [ onClick Submit, disabled (model.file == Nothing) ] [ text "Analyze FMU" ]
            , div [ Html.Attributes.style "white-space" "pre-wrap" ]
                -- style is necessary to preserve line breaks
                [ Html.h1 [] [ text "Status" ]
                , text model.status
                ]
            , div [ Html.Attributes.style "white-space" "pre-wrap" ]
                -- style is necessary to preserve line breaks
                [ Html.h1 [] [ text "Output" ]
                , text <|
                    case model.checkResult of
                        Nothing ->
                            "No output"

                        Just res ->
                            case res of
                                Ok value ->
                                    value.log

                                Err error ->
                                    error
                ]
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NParameter p ->
            case String.toInt p of
                Just i ->
                    ( { model | nParameter = String.fromInt <| clamp 10 100 i }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        LParameter p ->
            case String.toInt p of
                Just i ->
                    ( { model | lParameter = String.fromInt <| clamp 10 100 i }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        FileParameter files ->
            ( { model | file = List.head files }, Cmd.none )

        Submit ->
            ( model
            , case model.file of
                Nothing ->
                    Cmd.none

                Just f_ ->
                    Http.post
                        { url = ApiReq.correctURL model.basePath "api/fmuanalyzer"
                        , body = Http.multipartBody [ Http.filePart (File.name f_) f_, Http.stringPart "n" model.nParameter, Http.stringPart "l" model.lParameter ]
                        , expect = Http.expectJson SubmitResponse fmuanalyzerJsonDecoder
                        }
            )

        SubmitResponse result ->
            case result of
                Ok res ->
                    ( { model | checkResult = Just <| Ok res, id = res.faID }
                    , Http.get
                        { url = ApiReq.correctURL model.basePath "api/fmuanalyzer/?faID=" ++ res.faID
                        , expect =
                            Http.expectBytesResponse GetResultsResponse <|
                                \response ->
                                    case response of
                                        Http.GoodStatus_ metadata body ->
                                            Ok body

                                        _ ->
                                            Err Http.NetworkError
                        }
                    )

                Err err ->
                    ( { model | checkResult = Just (Err (Debug.toString err)) }, Cmd.none )

        GetResultsResponse result ->
            let
                deleteRequest =
                    Http.request
                        { method = "DELETE"
                        , headers = []
                        , url = ApiReq.correctURL model.basePath "api/general/?faID=" ++ model.id
                        , body = Http.emptyBody
                        , expect = Http.expectString DeleteResponse
                        , timeout = Nothing
                        , tracker = Nothing
                        }
            in
            case result of
                Ok bytes ->
                    ( model, Cmd.batch [ File.Download.bytes "results.zip" "application/zip" bytes, deleteRequest ] )

                Err err ->
                    ( { model | status = model.status ++ ("GetResultsReponse: " ++ Debug.toString err) }, deleteRequest )

        DeleteResponse result ->
            let
                newStatus =
                    model.status
                        ++ ("DeleteResponse: "
                                ++ (case result of
                                        Ok s ->
                                            s

                                        Err err ->
                                            Debug.toString err
                                   )
                           )
            in
            ( { model | status = newStatus }, Cmd.none )


type alias FMUAnalyzerResultJson =
    { log : String
    , out : String
    , err : String
    , faID : String
    }


fmuanalyzerJsonDecoder : D.Decoder FMUAnalyzerResultJson
fmuanalyzerJsonDecoder =
    D.map4 FMUAnalyzerResultJson
        (D.field "log" D.string)
        (D.field "out" D.string)
        (D.field "err" D.string)
        (D.field "faID" D.string)
