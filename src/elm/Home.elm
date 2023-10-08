module Home exposing (Model, Msg, init, update, view)

import Html exposing (Html, div, h1, text)
import Html.Attributes exposing (class)



-- model


type alias Model =
    {}


initModel : Model
initModel =
    {}


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )



-- update


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



-- view


view : Model -> Html Msg
view model =
    div [ class "container pt-3" ] [ h1 [] [ text "Home" ] ]
