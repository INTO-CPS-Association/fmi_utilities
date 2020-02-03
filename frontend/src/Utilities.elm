module Utilities exposing (filesDecoder, markdownToString)

import File exposing (..)
import Html exposing (..)
import Json.Decode as D
import Markdown.Parser



-- Even if multiple is False the result is always a list, just with 1 entry.
-- https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/file#files
-- "target" originate from: https://developer.mozilla.org/en-US/docs/Web/API/Event/target
-- "files" originate from: https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/file#Additional_attributes


filesDecoder : D.Decoder (List File)
filesDecoder =
    D.at [ "target", "files" ] (D.list File.decoder)



{- https://github.com/dillonkearns/elm-markdown/tree/2.0.0 -}


markdownToString : String -> Html msg
markdownToString mdText =
    case
        mdText
            |> Markdown.Parser.parse
            |> Result.mapError deadEndsToString
            |> Result.andThen (\ast -> Markdown.Parser.render Markdown.Parser.defaultHtmlRenderer ast)
    of
        Ok rendered ->
            div [] rendered

        Err errors ->
            text errors


deadEndsToString deadEnds =
    deadEnds
        |> List.map Markdown.Parser.deadEndToString
        |> String.join "\n"
