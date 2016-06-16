module Todo.App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Http
import Task
import Json.Decode as Json exposing ((:=), int, string)
import Json.Encode as E

import Jwt
import LocalStorage


-- Model


type alias Token =
  Maybe LocalStorage.Value


type alias Item =
  { message: String
  , url: String
  , update_at: String
  }


type alias InvalidContent =
  { result: String
  , reason: String
  }


type Todos
  = Valid (List Item)
  | Invalid InvalidContent


type alias Repo =
  { id: Int
  , name: String
  }


type Field
  = Uname
  | Pword


type alias Model =
  { registering: Bool
  , collapsed: Bool
  , todos: Todos
  ------ Info
  , app: String
  , repo: Maybe Repo
  ------ Register Form
  , uname: String
  , pword: String
  , errmsg : String
  }


init : String -> Todos -> Maybe Repo -> (Model, Cmd Msg)
init app todos repo =
  ( Model False True todos app repo "" "" ""
  , Cmd.none
  )




-- Update


type Msg
  = Collapse
  | Register
  ------ register form
  | FormInput Field String
  | SubmitRegister
  | RegSucceed String
  | RegFail Http.Error


update : Token -> Msg -> Model -> (Model, Cmd Msg)
update token msg model =
  case msg of
    Collapse ->
      { model | collapsed = not model.collapsed }
        ! []

    Register ->
      { model | registering = not model.registering }
        ! []

    FormInput inputId val ->
      let
        res = case inputId of
          Uname -> { model | uname = val }
          Pword -> { model | pword = val }
      in
        (res, Cmd.none)

    SubmitRegister ->
      model
        ! [ submit token model ]

    RegSucceed repo' ->
      case repo' of
        _ ->
          { model | repo = Nothing, registering = False }
            ! []

    RegFail err ->
      { model | repo = Nothing, errmsg = (toString err) }
        ! []





-- View


view : Model -> Html Msg
view model =
  div [ class "todo application" ]
      [ div [ class "header" ]
            [ i [ class "fa fa-th-large" ] []
            , text ("  " ++ model.app ++ todosCounter(model.todos))
            ]
      , (widget model)
      ]


todosCounter : Todos -> String
todosCounter todos =
  case todos of
    Invalid _ ->
      ""

    Valid todos' ->
      case List.length todos' of
        0 ->
          ""

        n ->
          "(" ++ (toString n) ++ ")"


widget : Model -> Html Msg
widget model =
  case model.repo of
    Nothing ->
      case model.todos of
        Valid todos' ->
          div [ class "content" ]
              [ itemsContainer todos' model.app ]
        Invalid error' ->
          div [ class "error content" ]
              [ text "获取失败" ]

    Just repo ->
      case model.registering of
        False ->
          div [ class "warning content" ]
              [
                p []
                  [
                    span [] [ text "您还没有授权获取该应用的待办，需要您进行授权登记。如需获取待办，请您" ]
                  ]
              , p []
                  [
                    a [ class "button-warning pure-button button-xsmall", onClick Register ] [ text "登记授权" ]
                  ]
              ]

        True ->
          div [ class "info content" ]
              [ regForm model.repo ]


itemsContainer : List Item -> String -> Html Msg
itemsContainer todos' app' =
  case List.length todos' of
    0 ->
      p [ class "item" ]
        [ i [ class "fa fa-thumbs-o-up" ] []
        , text "目前没有待办任务"
        ]

    _ ->
      div [] (List.map itemWidget todos')




itemWidget : Item -> Html Msg
itemWidget item =
  p [ class "item" ]
    [
      a [ href item.url, target "_blank" ]
        [
          i [ class "fa fa-dot-circle-o" ] []
        , text (item.message ++ " (" ++ item.update_at ++ ")")
        ]
    ]


regForm : Maybe Repo -> Html Msg
regForm maybeRepo =
  case maybeRepo of
    Nothing ->
      span [] []

    Just repo ->
      Html.form
        [ class "pure-form"
        , onSubmit SubmitRegister
        ]
        [ fieldset []
            [ legend [] [ text "授权获取应用的个人待办" ]
            , input
                [ type' "text"
                , placeholder (repo.name ++ "账号")
                , onInput (FormInput Uname) ]
                []
            , input
                [ type' "password"
                , placeholder (repo.name ++ "口令")
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
                , onClick Register
                , value "暂不授权" ]
                [ text "取消" ]
            ]
        ]





-- Subs


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none




-- Http

submit : Token -> Model -> Cmd Msg
submit token model =
  case model.repo of
    Nothing ->
      Cmd.none

    Just repo' ->
      case token of
        Nothing ->
          Cmd.none

        Just token' ->
          let
            url =
              "/api/v1/accounts"

            body' =
              E.object
                [ ( "accountRepoId",  E.int repo'.id )
                , ( "account", E.string model.uname )
                , ( "password", E.string model.pword )
                ]
                |> E.encode 0
                |> Http.string
          in
            Task.perform RegFail RegSucceed (Jwt.post token' string url body')


decodeRepo : Json.Decoder (Maybe Repo)
decodeRepo =
  Json.maybe
    <| Json.object2 Repo
        ("id" := int)
        ("name" := string)

