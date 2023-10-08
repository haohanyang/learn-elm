module Posts exposing (Model, Msg, init, update, view)

import Html exposing (Html, div, h1, h5, h6, text)
import Html.Attributes exposing (class)
import Http
import Json.Decode exposing (Decoder, field, int, list, map4, string)



-- model


type Model
    = Loading
    | Failure
    | Success (List Post)


type alias Post =
    { id : Int
    , title : String
    , body : String
    , userId : Int
    }


init : ( Model, Cmd Msg )
init =
    ( Loading
    , Http.get
        { url = "https://jsonplaceholder.typicode.com/posts"
        , expect = Http.expectJson GotPosts (list postDecoder)
        }
    )



-- update


type Msg
    = GotPosts (Result Http.Error (List Post))


postDecoder : Decoder Post
postDecoder =
    map4 Post
        (field "id" int)
        (field "title" string)
        (field "body" string)
        (field "userId" int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPosts (Ok posts) ->
            ( Success posts, Cmd.none )

        GotPosts (Err _) ->
            ( Failure, Cmd.none )



-- view


viewPost : Post -> Html Msg
viewPost post =
    div [ class "card mt-2" ]
        [ div [ class "card-body" ]
            [ h5 [ class "card-title" ] [ text post.title ]
            , h6 [ class "card-subtitle mb-2 text-body-secondary" ] [ text ("User" ++ String.fromInt post.userId) ]
            , div [ class "card-text" ] [ text post.body ]
            ]
        ]


view : Model -> Html Msg
view model =
    let
        content =
            case model of
                Loading ->
                    div [ class "spinner-border d-block mx-auto" ] []

                Failure ->
                    div [ class "alert alert-danger" ] [ text "Failed to load posts" ]

                Success posts ->
                    div [] (List.map viewPost posts)
    in
    div [ class "container pt-3" ] [ h1 [ class "mb-2" ] [ text "Posts" ], content ]
