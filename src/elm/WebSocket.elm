port module WebSocket exposing (Model, Msg, init, subscriptions, update, view)

import Html exposing (Html, button, div, h1, i, input, section, text)
import Html.Attributes exposing (class, classList, disabled, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode exposing (Decoder, bool, decodeString, field, map2, string)



-- model


type alias Message =
    { content : String
    , fromRemote : Bool
    }


type alias Model =
    { messages : List Message
    , input : String
    }


messageDecoder : Decoder Message
messageDecoder =
    map2 Message
        (field "content" string)
        (field "fromRemote" bool)


init : ( Model, Cmd Msg )
init =
    ( Model [] ""
    , Cmd.none
    )



-- ports


port sendMessage : String -> Cmd msg


port logError : String -> Cmd msg


port messageReceiver : (String -> msg) -> Sub msg



-- update


type Msg
    = ReceiveMessage String
    | SendMessage
    | EditMessage String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceiveMessage json ->
            case decodeString messageDecoder json of
                Ok message ->
                    ( { model | messages = message :: model.messages }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, logError ("Invalid message received: " ++ json) )

        SendMessage ->
            ( { model | input = "" }, sendMessage model.input )

        EditMessage message ->
            ( { model | input = message }
            , Cmd.none
            )



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    messageReceiver ReceiveMessage



-- view


viewMessage : Message -> Html Msg
viewMessage message =
    div [ classList [ ( "list-group-item", True ), ( "text-primary", not message.fromRemote ) ] ]
        [ text message.content ]


view : Model -> Html Msg
view model =
    div [ class "container pt-3" ]
        [ h1 [ class "mb-2" ] [ text "WebSocket Echo Chat" ]
        , section [ class "card chat-messages mb-3" ]
            [ div [ class "card-header fw-bold" ] [ text "Messages" ]
            , div [ class "list-group list-group-flush" ] (List.map viewMessage (List.reverse model.messages))
            ]
        , section [ class "input-group" ]
            [ input [ type_ "text", class "form-control", onInput EditMessage, value model.input ] []
            , button
                [ class "btn btn-primary"
                , type_ "button"
                , onClick SendMessage
                , disabled (String.isEmpty model.input)
                ]
                [ i [ class "bi bi-send" ] [] ]
            ]
        ]
