module Todo.Authorize exposing (..)


import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Task
import Http
import String

import Json.Decode as Json exposing ((:=))
import Json.Encode as E

import Jwt

--import Applink.Common exposing (..)
import Common exposing (..)




-- model


type Field
  = Uname
  | Pword


type alias Auth =
  { repoId: Int
  , uid: String
  , uname: String
  }


type alias Model =
  { repoId: Int
  , uname: String
  , pword: String
  , remoting: Bool
  , errmsg: String
  }


title : String
title =
  "待办获取授权"


initEmpty : Model
initEmpty =
  init 0


init : Int -> Model
init repoId =
  { repoId = repoId
  , uname = ""
  , pword = ""
  , remoting = False
  , errmsg = ""
  }




-- update


type Msg
  = UpdateRemote
  | CancelRemote
  | UpdateSucceed Auth
  | UpdateFail Http.Error
  ------
  | FormInput Field String



update : Url -> Token -> Msg -> Model -> (Model, Cmd Msg)
update url token msg model =
  let _ = Debug.log "...." model in

  case msg of
    UpdateRemote ->
      case checkFields model of
        (True, _) ->
          { model | remoting = True }
          ! [ updateRemote url token model ]

        (False, tips) ->
          { model | errmsg = tips } ! []


    CancelRemote ->
      { model | remoting = False }
      ! []


    UpdateSucceed auth ->
      { model | remoting = False }
      ! []


    UpdateFail err ->
      { model | errmsg = (toString err), remoting = False }
      ! []


    FormInput inputId val ->
      let
        res = case inputId of
          Uname     -> { model | uname = val }
          Pword     -> { model | pword = val }
      in
        { res | errmsg = "" } ! []



checkFields : Model -> (Bool, String)
checkFields model =
  if String.isEmpty model.uname then
    (False, "请填写账号")

  else if String.isEmpty model.pword then
    (False, "请填写口令")

  else
    (True, "")



-- view


view : Model -> Html Msg
view model =
  div []
    [ formWidget title model.errmsg
    ]


errorWidget : String -> Html msg
errorWidget errmsg =
  case errmsg of
    "" ->
      div [] []

    err ->
      div
        [ class "error" ]
        [ text err ]


formWidget : String -> String -> Html Msg
formWidget title errmsg =
  Html.form
    [ class "pure-form"
    , onSubmit UpdateRemote
    ]
    [ fieldset []
        [ legend [] [ text title ]
        , errorWidget errmsg
        , input
            [ type' "text"
            , placeholder "账号"
            , onInput (FormInput Uname) ]
            []
        , input
            [ type' "password"
            , placeholder "口令"
            , onInput (FormInput Pword) ]
          []
        , input
            [ class "pure-button pure-button-primary button-xsmall"
            , type' "submit"
            , value "登记" ]
            []
        , input
            [ class "pure-button button-warning button-xsmall"
            , type' "cancel"
            , onClick CancelRemote
            , value "暂不授权" ]
            [ text "取消" ]
        ]
    ]




-- subs


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none




-- http

updateRemote : Url -> Token -> Model -> Cmd Msg
updateRemote url token model =
  let _ = Debug.log "url ..." url
      _ = Debug.log "token ...." token
      _ = Debug.log "model ...." model
  in


  case token of
    Nothing ->
      Cmd.none

    Just token' ->
      let
        _ = Debug.log "...." token'

        body =
          E.object
            [ ("accountRepoId", E.int model.repoId)
            , ("account", E.string model.uname)
            , ("password", E.string model.pword)
            ]
            |> E.encode 0
            |> Http.string

        task =
          Jwt.post token' decodeAuth url body
      in
        Task.perform UpdateFail UpdateSucceed task



decodeAuth : Json.Decoder Auth
decodeAuth =
  Json.object3 Auth
    ("accountRepoId" := Json.int)
    ("userId" := Json.string)
    ("account" := Json.string)

