module Page.Home exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Utilities


type alias Model =
    ()


type alias Msg =
    ()


init : ( Model, Cmd Msg )
init =
    ( (), Cmd.none )


textBody : String
textBody =
    """
This website is the frontend for various FMI-related activities emerging from the INTO-CPS Association.

Information on the activities the are representing on this website is on the page of the activity in question.\\
Currently the activities are the following: 
- **FMI Checker** checks the static semantics of a model description file of an FMU.
- **FMU Analyzer** exercises an FMU by loading it an invoking various functions. It takes the model description file of a given FMU into consideration when setting and getting variables.

For website issues, please report these at: [https://github.com/into-cps-association/fmi_utilities/issues](https://github.com/into-cps-association/fmi_utilities/issues.)        

For issues related to the tools, please report use the links within the page of the tool.
"""


view : Model -> Html Msg
view _ =
    div [ class "card" ]
        [ div [ class "card-header" ] [ text "Home" ]
        , div [ class "card-body" ]
            [ Utilities.markdownToString textBody
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )
