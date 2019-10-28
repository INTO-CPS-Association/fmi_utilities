module Routes exposing (..)

import Debug as Debug
import Url
import Url.Parser exposing (..)


type Route
    = HomeRoute
    | FMICheckerRoute
    | FMUAnalyzerRoute
    | NotFoundRoute


fromUrl : String -> Url.Url -> Route
fromUrl basePath url =
    { url | path = String.replace basePath "" url.path }
        |> parse matchers
        |> Maybe.withDefault NotFoundRoute


matchers : Parser (Route -> a) a
matchers =
    oneOf
        [ map HomeRoute top
        , map FMICheckerRoute (s "fmichecker")
        , map FMUAnalyzerRoute (s "fmuanalyzer")
        ]


pathFor : Route -> String
pathFor route =
    case route of
        FMUAnalyzerRoute ->
            "fmuanalyzer"

        FMICheckerRoute ->
            "fmichecker"

        HomeRoute ->
            ""

        NotFoundRoute ->
            ""
