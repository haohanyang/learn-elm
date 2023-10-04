module Counter exposing (main)

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Value 0, Cmd.none )


type Model
    = Value Int


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Value i) =
    case msg of
        Increment ->
            ( Value (i + 1), Cmd.none )

        Decrement ->
            ( Value (i - 1), Cmd.none )


view : Model -> Document Msg
view (Value i) =
    { title = "Counter"
    , body =
        [ div [ class "container" ]
            [ button [ onClick Decrement ] [ text "-" ]
            , div [] [ text (String.fromInt i) ]
            , button [ onClick Increment ] [ text "+" ]
            ]
        ]
    }
