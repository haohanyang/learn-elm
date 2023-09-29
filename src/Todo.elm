module Todo exposing (main)

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



-- Model


type alias Model =
    { entries : List Entry
    , current : String
    , uid : Int
    }


type alias Entry =
    { id : Int
    , countent : String
    , finished : Bool
    , editing : Bool
    , editDraft : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model [] "" 0, Cmd.none )


initEntry : Int -> String -> Entry
initEntry id content =
    { id = id, countent = content, finished = False, editing = False, editDraft = content }



-- Update


type Msg
    = WriteCurrent String
    | AddEntry
    | DeleteEntry Int
    | ToggleFinished Int
    | EditWriteContent Int String
    | SaveChange Int
    | ToggleEditEntry Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WriteCurrent str ->
            ( { model | current = str }, Cmd.none )

        AddEntry ->
            if model.current == "" then
                ( model, Cmd.none )

            else
                ( { model
                    | entries = initEntry model.uid model.current :: model.entries
                    , current = ""
                    , uid = model.uid + 1
                  }
                , Cmd.none
                )

        DeleteEntry id ->
            ( { model | entries = List.filter (\entry -> entry.id /= id) model.entries }
            , Cmd.none
            )

        ToggleFinished _ ->
            ( { model
                | entries =
                    List.map (updateEntry msg) model.entries
              }
            , Cmd.none
            )

        SaveChange _ ->
            ( { model
                | entries =
                    List.map (updateEntry msg) model.entries
              }
            , Cmd.none
            )

        EditWriteContent _ _ ->
            ( { model
                | entries =
                    List.map (updateEntry msg) model.entries
              }
            , Cmd.none
            )

        ToggleEditEntry _ ->
            ( { model
                | entries =
                    List.map (updateEntry msg) model.entries
              }
            , Cmd.none
            )


updateEntry : Msg -> Entry -> Entry
updateEntry msg entry =
    case msg of
        ToggleFinished i ->
            if entry.id == i then
                { entry | finished = not entry.finished }

            else
                entry

        ToggleEditEntry i ->
            if entry.id == i then
                { entry | editing = not entry.editing, editDraft = entry.countent }

            else
                entry

        SaveChange i ->
            if entry.id == i then
                { entry | countent = entry.editDraft, editing = False }

            else
                entry

        EditWriteContent i content ->
            if entry.id == i then
                { entry | editDraft = content }

            else
                entry

        _ ->
            entry


viewEntry : Entry -> Html Msg
viewEntry entry =
    li []
        [ input
            [ type_ "checkbox"
            , checked entry.finished
            , onClick (ToggleFinished entry.id)
            ]
            []
        , span
            [ class
                ((if entry.finished then
                    "finished"

                  else
                    ""
                 )
                    ++ (if entry.editing then
                            " hidden"

                        else
                            ""
                       )
                )
            ]
            [ text entry.countent ]
        , input
            [ type_ "text"
            , value entry.editDraft
            , class
                (if entry.editing then
                    ""

                 else
                    "hidden"
                )
            , onInput (EditWriteContent entry.id)
            ]
            []
        , button [ onClick (DeleteEntry entry.id) ] [ text "delete" ]
        , button [ onClick (ToggleEditEntry entry.id) ]
            [ text
                (if entry.editing then
                    "cancel"

                 else
                    "edit"
                )
            ]
        , button
            [ onClick (SaveChange entry.id)
            , class
                (if entry.editing then
                    ""

                 else
                    "hidden"
                )
            ]
            [ text "save" ]
        ]


view : Model -> Document Msg
view model =
    { title = "Todo"
    , body =
        [ input [ type_ "text", value model.current, onInput WriteCurrent ] []
        , button [ onClick AddEntry ]
            [ text "add" ]
        , ul
            []
            (List.map viewEntry model.entries)
        ]
    }
