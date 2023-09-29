module Todo exposing (..)

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


program : Program () Model Msg
program =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- Model


type alias Model =
    { entries : List Entry
    , current : String
    , uid : Int
    }


type alias Entry =
    { id : Int
    , countent : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model [] "" 0, Cmd.none )



-- Update


type Msg
    = WriteCurrent String
    | AddEntry


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WriteCurrent str ->
            ( { model | current = str }, Cmd.none )

        AddEntry ->
            let
                entry =
                    { id = model.uid, countent = model.current }
            in
            ( { model | entries = entry :: model.entries, current = "", uid = model.uid + 1 }, Cmd.none )


view : Model -> Document Msg
view model =
    { title = "Elm Todo"
    , body =
        [ input [ type_ "text", value model.current, onInput WriteCurrent ] []
        , button [ onClick AddEntry ]
            [ text "Add" ]
        , ul
            []
            (List.map (\e -> li [] [ text e.countent ]) model.entries)
        ]
    }
