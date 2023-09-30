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
    let
        deleteButton =
            button [ onClick (DeleteEntry entry.id), class "btn btn-outline-danger" ]
                [ i [ class "bi bi-trash" ] [] ]

        editButton =
            button [ onClick (ToggleEditEntry entry.id), class "btn btn-outline-secondary" ]
                [ i
                    [ class
                        (if entry.editing then
                            "bi bi-x-lg"

                         else
                            "bi bi-pencil"
                        )
                    ]
                    []
                ]

        saveButton =
            button
                [ onClick (SaveChange entry.id)
                , class "btn btn-outline-success"
                ]
                [ i [ class "bi bi-check-lg" ] []
                ]

        buttons =
            if entry.editing then
                [ deleteButton, editButton, saveButton ]

            else
                [ deleteButton, editButton ]
    in
    li [ class "list-group-item d-flex justify-content-between align-items-center" ]
        [ div []
            [ input
                [ type_ "checkbox"
                , checked entry.finished
                , onClick (ToggleFinished entry.id)
                , class
                    ("form-check-input "
                        ++ (if entry.editing then
                                "d-none"

                            else
                                ""
                           )
                    )
                , id ("checkbox-" ++ String.fromInt entry.id)
                ]
                []
            , label
                [ class
                    ("ml-2 "
                        ++ (if entry.finished then
                                "text-decoration-line-through "

                            else
                                ""
                           )
                        ++ (if entry.editing then
                                "d-none"

                            else
                                ""
                           )
                    )
                , for ("checkbox-" ++ String.fromInt entry.id)
                ]
                [ text entry.countent ]
            , input
                [ type_ "text"
                , value entry.editDraft
                , class
                    ("form-control "
                        ++ (if entry.editing then
                                ""

                            else
                                "d-none"
                           )
                    )
                , onInput (EditWriteContent entry.id)
                ]
                []
            ]
        , div [ class "btn-group" ] buttons
        ]


view : Model -> Document Msg
view model =
    { title = "Todo"
    , body =
        [ div [ class "container mt-5" ]
            [ h1 [ class "text-center" ] [ text "Todo List" ]
            , section [ class "input-group mt-3" ]
                [ input [ type_ "text", value model.current, class "form-control", onInput WriteCurrent ] []
                , button [ onClick AddEntry, class "btn btn-primary" ]
                    [ text "add" ]
                ]
            , section [ class "mt-3" ]
                [ ul
                    [ class "list-group" ]
                    (List.map viewEntry model.entries)
                ]
            ]
        ]
    }
