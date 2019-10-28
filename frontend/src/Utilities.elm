module Utilities exposing (filesDecoder)

import File exposing (..)
import Json.Decode as D



-- Even if multiple is False the result is always a list, just with 1 entry.
-- https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/file#files


filesDecoder : D.Decoder (List File)
filesDecoder =
    D.at [ "target", "files" ] (D.list File.decoder)



-- "target" originate from: https://developer.mozilla.org/en-US/docs/Web/API/Event/target
-- "files" originate from: https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/file#Additional_attributes
