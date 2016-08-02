module Login exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as HtmlApp

import Http
import Task
import Json.Decode as Json exposing ( (:=), Value )
import Json.Encode as E

import Jwt exposing (..)
import LocalStorage




--MODEL

type Field
  = Uname
  | Pword


type alias Model
  = { uname: String
    , pword: String
    , token: Maybe LocalStorage.Value
    , errmsg: String
    }


type alias JwtToken
  = { uid: String
    , exp: Int
    }

tokenDecoder =
    Json.object2 JwtToken
        ("uid" := Json.string)
        ("exp" := Json.int)


init : (Model, Cmd Msg)
init = (Model "" "" Nothing "", Cmd.none)





-- UPDATE


type Msg
  = FormInput Field String
  | Submit
  ---------
  | LoginSuccess (Result JwtError String)
  | LoginFail JwtError
  ---------
  | TokenSucceed ()
  | TokenError LocalStorage.Error


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    FormInput inputId val ->
      let res = case inputId of
        Uname -> { model | uname = val, errmsg = "" }
        Pword -> { model | pword = val, errmsg = "" }
      in (res, Cmd.none)

    Submit ->
      let credentials =
        E.object  [ ("uid",     E.string model.uname)
                  , ("passwd",  E.string model.pword)
                  ]
        |> E.encode 0
      in
        ( model
        , Task.perform
            LoginFail LoginSuccess
              ( Jwt.authenticate
                  ("token" := Json.string)
                  "/api/v1/auth"
                  credentials
                |> Task.toResult
              )
          )

    LoginSuccess passToken ->
      case passToken of
        Result.Ok token' ->
          { model | token = (Just token'), errmsg = "" }
            ! [ Task.perform TokenError TokenSucceed (LocalStorage.set "jwt-token" token')]
        Result.Err err ->
          case err of
            HttpError (Http.BadResponse 401 _) ->
              { model | errmsg = "认证失败，请重新登录，并注意大小写" } ! []

            _ ->
              { model | errmsg = toString err } ! []

    LoginFail err ->
      { model | errmsg = toString err } ! []

    TokenSucceed () ->
      model ! []

    TokenError err ->
      { model | errmsg = toString err } ! []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none




-- VIEW

view : Model -> Html Msg
view model =
  div [ class "pure-g login" ]
      [ div [ class "pure-u-1-4" ] []
      , div [ class "pure-u-1-2" ]
            [ p [ class "background-line" ]
                [ span [ class "splash-head" ] [ text "登录" ] ]
            , div [ class "pure-g" ]
                  [ div [ class "pure-u-1-4" ] []
                  , div [ class "pure-u-1-2" ]
                    [ loginForm model ]
                  ]
            ]
      ]



loginForm : Model -> Html Msg
loginForm model =
  Html.form [ onSubmit Submit, class "pure-form" ]
            [ fieldset  [ class "pure-group" ]
                        [ input [ onInput (FormInput Uname)
                                , type' "text"
                                , class "pure-input-1"
                                , placeholder "账号"
                                ]
                                [ text model.uname ]
                        , input [ onInput (FormInput Pword)
                                , type' "password"
                                , class "pure-input-1"
                                , placeholder "密码"
                                ]
                                [ text model.pword ]
                        ]
                        , errmsg model.errmsg
                        , input [ type' "submit"
                                , value "登录"
                                , class "pure-button pure-input-1 pure-button-primary"
                                ]
                                []
            ]


errmsg : String -> Html msg
errmsg err =
  case err of
    "" -> span [] []

    _  -> div
            [ class "pure-u-1 error" ]
            [ i [ class "fa fa-1x fa-exclamation-circle icon-prefix" ] []
            , text err
            ]

