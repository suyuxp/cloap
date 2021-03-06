module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as App
import Http
import Json.Decode as Json exposing ((:=))
import Navigation
import String
import Task
import UrlParser exposing (Parser, (</>), format, int, oneOf, s, string)

import Login
import Todo exposing (..)
import LocalStorage


main =
  Navigation.program (Navigation.makeParser hashParser)
    { init = init
    , view = view
    , update = update
    , urlUpdate = urlUpdate
    , subscriptions = subscriptions
    }



-- URL PARSERS - check out evancz/url-parser for fancier URL parsing


toHash : Page -> String
toHash page =
  case page of
    Home ->
      "#home"

    Login ->
      "#login"


hashParser : Navigation.Location -> Result String Page
hashParser location =
  UrlParser.parse identity pageParser (String.dropLeft 1 location.hash)


type Page = Home | Login


pageParser : Parser (Page -> a) a
pageParser =
  oneOf
    [ format Home (UrlParser.s "home")
    , format Login (UrlParser.s "login")
    ]





-- MODEL


type alias Model =
  { page : Page
  , login : Login.Model
  , todo : Todo.Model
  , jwtToken : Maybe LocalStorage.Value
  }


init : Result String Page -> (Model, Cmd Msg)
init result =
  let
    ( todoModel, todoCmds ) =
      Todo.init

    ( urlModel, urlCmds ) =
      urlUpdate result (Model Home (fst Login.init) todoModel Nothing)
  in
    ( urlModel
    , Cmd.batch
        [ Cmd.map TodoPage todoCmds
        , urlCmds
        , Task.perform TokenError TokenSucceed (LocalStorage.get "jwt-token")
        ]
    )





-- UPDATE


type Msg
  = LoginPage Login.Msg
  | Logout
  | LogoutError LocalStorage.Error
  | LogoutSucceed ()
  -----
  | TodoPage Todo.Msg
  -----
  | TokenError LocalStorage.Error
  | TokenSucceed (Maybe LocalStorage.Value)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    LoginPage msg' ->
      case model.login.token of
        Nothing ->
          let
            (model', cmds') =
              Login.update msg' model.login
          in
            ( { model | login = model' }
            , Cmd.map LoginPage cmds'
            )

        Just token' ->
          { model | jwtToken = (Just token'), page = Home }
            ! [ Navigation.modifyUrl (toHash Home) ]

    Logout ->
      case model.jwtToken of
        Nothing ->
          model ! []

        Just token' ->
          model ! [ Task.perform LogoutError LogoutSucceed (LocalStorage.remove "jwt-token") ]

    LogoutError err' ->
      model ! []

    LogoutSucceed _ ->
      init (Result.Ok Login)



    TodoPage msg' ->
      let
        (todoModel, todoCmds) =
          Todo.update model.jwtToken msg' model.todo
      in
        ( { model | todo = todoModel }
        , Cmd.map TodoPage todoCmds
        )

    ----------------

    TokenSucceed token' ->
      let
        cmd'' =
          case model.page of
            Home ->
              let
                (model', cmd') =
                  Todo.update token' Todo.Fetch model.todo
              in
                Cmd.map TodoPage cmd'

            _ ->
              Cmd.none
      in
        ( { model | jwtToken = token' }
        , cmd''
        )

    TokenError _ ->
      { model | jwtToken = Nothing }
        ! []



{-| The URL is turned into a result. If the URL is valid, we just update our
model to the new count. If it is not a valid URL, we modify the URL to make
sense.
-}
urlUpdate : Result String Page -> Model -> (Model, Cmd Msg)
urlUpdate result model =
  case result of
    Err _ ->
      ( model, Navigation.modifyUrl (toHash model.page) )

    Ok Home ->
      { model | page = Home }
        ! [ Cmd.map TodoPage (snd <| Todo.update model.jwtToken Todo.Fetch model.todo)]

    Ok page ->
      { model | page = page }
        ! []




-- SUBS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Sub.map LoginPage (Login.subscriptions model.login)
    , Sub.map TodoPage (Todo.subscriptions model.todo)
    ]




-- VIEW


view : Model -> Html Msg
view model =
  div [ ]
    [ div
      [ class "header" ]
      [ div
          [ class "home-menu pure-menu pure-menu-horizontal pure-menu-fixed" ]
          [ a
              [ class "pure-menu-heading", href (toHash Home) ]
              [ img [ src "images/logo.png" ] [] ]
          , ul
              [ class "pure-menu-list" ]
              [ linkTo Home "first-order" "待办"
              , loginLink model ]
              ]
          ]
      , div
          [ class "pure-container" ]
          ( viewPage model )
    ]


linkTo : Page -> String -> String -> Html Msg
linkTo page icon label =
  li
    [ class "pure-menu-item" ]
    [ a [ class "pure-menu-link", href (toHash page) ]
        [ i [ class ("fa fa-" ++ icon) ] []
        , text label
        ]
    ]


loginLink : Model -> Html Msg
loginLink model =
  case model.jwtToken of
    Nothing ->
      linkTo Login "sign-in" "登录"

    Just _ ->
      li
        [ class "pure-menu-item" ]
        [ a [ class "pure-menu-link", onClick Logout ]
            [ i [ class "fa fa-sign-out" ] []
            , text "退出"
            ]
        ]


viewPage : Model -> List (Html Msg)
viewPage model =
  case model.page of
    Home ->
      [
        App.map TodoPage (Todo.view model.jwtToken model.todo)
      ]

    Login ->
      case model.jwtToken of
        Nothing ->
          [ App.map LoginPage (Login.view model.login) ]

        Just token ->
          [ div [ class "splash-container" ]
                [ div [ class "splash" ]
                      [ p [ class "background-line" ]
                          [ span [ class "splash-head" ] [ text "登录成功" ] ]
                      ]
                ]
          ]



