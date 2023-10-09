module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Home exposing (Model, Msg, init, view)
import Html exposing (Html, a, button, div, h1, li, nav, span, text, ul)
import Html.Attributes exposing (attribute, class, classList, href, id, type_)
import Html.Lazy exposing (lazy)
import Posts exposing (Model, Msg, init, view)
import Todo exposing (Model, Msg, init, view)
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, s)


main : Program (Maybe Todo.Model) Model Msg
main =
    Browser.application
        { init = init
        , subscriptions = subscriptions
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        , update = update
        , view = view
        }


type Route
    = Home
    | Posts
    | Todos


type Page
    = HomePage Home.Model
    | PostsPage Posts.Model
    | TodosPage Todo.Model
    | NotFoundPage


parser : Parser (Route -> a) a
parser =
    Parser.oneOf
        [ Parser.map Home Parser.top
        , Parser.map Home (s "home")
        , Parser.map Posts (s "posts")
        , Parser.map Todos (s "todos")
        ]


init : Maybe Todo.Model -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    updateUrl url { page = NotFoundPage, key = key, initTodos = flags }


updateUrl : Url -> Model -> ( Model, Cmd Msg )
updateUrl url model =
    case Parser.parse parser url of
        Just Home ->
            Home.init |> toHome model

        Just Posts ->
            Posts.init |> toPosts model

        Just Todos ->
            Todo.init model.initTodos |> toTodos model

        Nothing ->
            ( model, Cmd.none )



-- MODEL


type alias Model =
    { page : Page, key : Nav.Key, initTodos : Maybe Todo.Model }



-- UPDATE


type Msg
    = ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | GotHomesMsg Home.Msg
    | GotPostsMsg Posts.Msg
    | GotTodosMsg Todo.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedLink (Browser.External href) ->
            ( model, Nav.load href )

        ChangedUrl url ->
            updateUrl url model

        ClickedLink (Browser.Internal url) ->
            ( model, Nav.pushUrl model.key (Url.toString url) )

        GotHomesMsg homeMsg ->
            case model.page of
                HomePage homeModel ->
                    toHome model (Home.update homeMsg homeModel)

                _ ->
                    ( model, Cmd.none )

        GotPostsMsg postsMsg ->
            case model.page of
                PostsPage postsModel ->
                    toPosts model (Posts.update postsMsg postsModel)

                _ ->
                    ( model, Cmd.none )

        GotTodosMsg todoMsg ->
            case model.page of
                TodosPage todoModel ->
                    toTodos model (Todo.updateWithStorage todoMsg todoModel)

                _ ->
                    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


viewHeader : Page -> Html Msg
viewHeader page =
    let
        links =
            [ navLink Home { url = "/", caption = "Home" }
            , navLink Posts { url = "/posts", caption = "Posts" }
            , navLink Posts { url = "/todos", caption = "Todos" }
            ]

        navLink : Route -> { url : String, caption : String } -> Html msg
        navLink route { url, caption } =
            li [ class "nav-item" ]
                [ a
                    [ classList
                        [ ( "nav-link", True )
                        , ( "active", isActive { link = route, page = page } )
                        ]
                    , href url
                    ]
                    [ text caption ]
                ]
    in
    nav [ class "navbar navbar-expand-lg bg-dark", attribute "data-bs-theme" "dark" ]
        [ div [ class "container-fluid" ]
            [ a [ class "navbar-brand", href "#" ]
                [ text "Elm SPA" ]
            , button
                [ class "navbar-toggler"
                , type_ "button"
                , attribute "data-bs-toggle" "collapse"
                , attribute "data-bs-target" "#navlinks"
                ]
                [ span [ class "navbar-toggler-icon" ] [] ]
            , div [ class "collapse navbar-collapse", id "navlinks" ]
                [ ul
                    [ class "navbar-nav" ]
                    links
                ]
            ]
        ]


isActive : { link : Route, page : Page } -> Bool
isActive { link, page } =
    case ( link, page ) of
        ( Home, HomePage _ ) ->
            True

        ( Posts, PostsPage _ ) ->
            True

        _ ->
            False


toHome : Model -> ( Home.Model, Cmd Home.Msg ) -> ( Model, Cmd Msg )
toHome model ( homeModel, cmd ) =
    ( { model | page = HomePage homeModel }, Cmd.map GotHomesMsg cmd )


toPosts : Model -> ( Posts.Model, Cmd Posts.Msg ) -> ( Model, Cmd Msg )
toPosts model ( postsModel, cmd ) =
    ( { model | page = PostsPage postsModel }, Cmd.map GotPostsMsg cmd )


toTodos : Model -> ( Todo.Model, Cmd Todo.Msg ) -> ( Model, Cmd Msg )
toTodos model ( todoModel, cmd ) =
    ( { model | page = TodosPage todoModel }, Cmd.map GotTodosMsg cmd )


viewContent : Page -> Html Msg
viewContent page =
    case page of
        HomePage homeModel ->
            Home.view homeModel |> Html.map GotHomesMsg

        PostsPage postsModel ->
            Posts.view postsModel |> Html.map GotPostsMsg

        TodosPage todoModel ->
            Todo.view todoModel |> Html.map GotTodosMsg

        NotFoundPage ->
            div [ class "container" ] [ h1 [] [ text "Not Found" ] ]


view : Model -> Document Msg
view model =
    { title = "Elm SPA Example"
    , body = [ lazy viewHeader model.page, viewContent model.page ]
    }
