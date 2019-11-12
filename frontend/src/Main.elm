module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Navigation as Nav
import Debug as Debug
import File exposing (File)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D
import Page.FMIChecker as FMIChecker
import Page.FMUAnalyzer as FMUAnalyzer
import Page.Home as Home
import Routes as Routes
import Url



--https://github.com/sporto/elm-example-app/blob/master/src/Main.elm
-- https://guide.elm-lang.org/webapps/navigation.html
-- MAIN


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MODEL


type alias Flags =
    { basePath : String }


type Page
    = Home Home.Model
    | FMIChecker FMIChecker.Model
    | FMUAnalyzer FMUAnalyzer.Model
    | PageNone


type alias Model =
    { navKey : Nav.Key
    , url : Url.Url
    , basePath : String
    , route : Routes.Route
    , currentPage : Page
    }


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | HomeMsg Home.Msg
    | FMICheckerMsg FMIChecker.Msg
    | FMUAnalyzerMsg FMUAnalyzer.Msg


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        model =
            { navKey = key, url = url, basePath = flags.basePath, route = Routes.fromUrl flags.basePath url, currentPage = PageNone }
    in
    Debug.log ("basePath" ++ flags.basePath)
        ( model, Cmd.none )
        |> loadCurrentPage


loadCurrentPage : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
loadCurrentPage ( model, cmd ) =
    let
        ( page, newCmd ) =
            case model.route of
                Routes.HomeRoute ->
                    let
                        ( h_model, h_cmd ) =
                            Home.init
                    in
                    ( Home h_model, Cmd.map HomeMsg h_cmd )

                Routes.FMICheckerRoute ->
                    let
                        ( f_model, f_cmd ) =
                            FMIChecker.init model.basePath
                    in
                    ( FMIChecker f_model, Cmd.map FMICheckerMsg f_cmd )

                Routes.FMUAnalyzerRoute ->
                    let
                        ( f_model, f_cmd ) =
                            FMUAnalyzer.init model.basePath
                    in
                    ( FMUAnalyzer f_model, Cmd.map FMUAnalyzerMsg f_cmd )

                Routes.NotFoundRoute ->
                    ( PageNone, Cmd.none )
    in
    ( { model | currentPage = page }, newCmd )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.navKey (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            let
                newRoute =
                    Routes.fromUrl model.basePath url
            in
            ( { model | url = url, route = newRoute }
            , Cmd.none
            )
                |> loadCurrentPage

        HomeMsg h_msg ->
            case model.currentPage of
                Home h_model ->
                    let
                        ( newPageModel, newCmd ) =
                            Home.update h_msg h_model
                    in
                    ( { model | currentPage = Home newPageModel }, Cmd.map HomeMsg newCmd )

                _ ->
                    ( model, Cmd.none )

        FMICheckerMsg f_msg ->
            case model.currentPage of
                FMIChecker f_model ->
                    let
                        ( newPageModel, newCmd ) =
                            FMIChecker.update f_msg f_model
                    in
                    ( { model | currentPage = FMIChecker newPageModel }, Cmd.map FMICheckerMsg newCmd )

                _ ->
                    ( model, Cmd.none )

        FMUAnalyzerMsg f_msg ->
            case model.currentPage of
                FMUAnalyzer f_model ->
                    let
                        ( newPageModel, newCmd ) =
                            FMUAnalyzer.update f_msg f_model
                    in
                    ( { model | currentPage = FMUAnalyzer newPageModel }, Cmd.map FMUAnalyzerMsg newCmd )

                _ ->
                    ( model, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "INTO-CPS Association Utilities"
    , body =
        [ div [ class "container-fluid" ]
            [ h1 [ class "text-center" ]
                [ text "INTO-CPS Association Utilities" ]
            ]
        , div
            [ class "container" ]
            [ div
                []
                [ renderMenu model ]
            , div
                [ class "row jumbotron my-auto" ]
                [ renderPage model ]
            , div
                [ class "text-center" ]
                [ text "Copyright © INTO-CPS Association" ]
            ]
        ]
    }



{-
   <nav class="navbar navbar-expand-lg navbar-light bg-light">
-}


renderMenu : Model -> Html Msg
renderMenu model =
    div [ class "navbar, navbar-expand-lg" ]
        [ ul [ class "navbar-nav nav-fill w-100" ]
            [ li [ class "nav-item" ] [ a [ classList [ ( "nav-link", True ), ( "active", model.route == Routes.HomeRoute ) ], href (Routes.pathFor Routes.HomeRoute) ] [ text "Home" ] ]
            , li [ class "nav-item" ] [ a [ classList [ ( "nav-link", True ), ( "active", model.route == Routes.FMICheckerRoute ) ], href (Routes.pathFor Routes.FMICheckerRoute) ] [ text "FMIChecker" ] ]
            , li [ class "nav-item" ] [ a [ classList [ ( "nav-link", True ), ( "active", model.route == Routes.FMUAnalyzerRoute ) ], href (Routes.pathFor Routes.FMUAnalyzerRoute) ] [ text "FMUAnalyzer" ] ]
            ]
        ]


renderPage : Model -> Html Msg
renderPage model =
    let
        currentPage =
            case model.currentPage of
                Home h_model ->
                    Home.view h_model |> Html.map HomeMsg

                FMIChecker f_model ->
                    FMIChecker.view f_model |> Html.map FMICheckerMsg

                FMUAnalyzer f_model ->
                    FMUAnalyzer.view f_model |> Html.map FMUAnalyzerMsg

                PageNone ->
                    Debug.log ("Main->renderPage->PageNon->model " ++ Debug.toString model) text "Invalid URL"
    in
    currentPage
