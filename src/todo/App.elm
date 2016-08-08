module Todo.App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Http
import Task
import Json.Decode as Json exposing ((:=), int, string, bool)
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
  , flag: String
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


type alias Links =
  { priorityUp: String
  , priorityDown: String
  }


type alias RefreshModel =
  { updated_at: Int
  , todos: List Item
  , refresh: String
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
  , appId: Int
  , repoId: Int
  , homepage: String
  , updateAt: String
  , defaultShow: Int
  , links: Links
  , repo: Maybe Repo
  ------ Register Form
  , uname: String
  , pword: String
  , errmsg : String
  ------ Refresh
  , refreshing: Bool
  }


init : String -> Int -> Int -> String -> String -> Links -> Todos -> Maybe Repo -> (Model, Cmd Msg)
init app appId repoId homepage updateAt links todos repo =
  ( Model False True todos app appId repoId homepage updateAt 5 links repo "" "" "" False
  , Cmd.none
  )




-- Update


type Msg
  = Collapse
  | Register
  ------ Admin
  | UpPriority Todos
  | DownPriority Todos
  | SetDefaultShowSucceed Int
  | AdminFail Http.Error
  ------ register form
  | FormInput Field String
  | SubmitRegister
  | RegSucceed String
  | RegFail Http.Error
  ------ refresh
  | Refresh
  | RefreshSucceed RefreshModel
  | RefreshFail Http.Error


update : Token -> Msg -> Model -> (Model, Cmd Msg)
update token msg model =
  let _ = Debug.log "....." msg in
  case msg of
    Collapse ->
      { model | collapsed = not model.collapsed }
        ! []

    Register ->
      let _ = Debug.log "...." model in
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


    UpPriority _ ->
      model ! []


    DownPriority _ ->
      model ! []


    SetDefaultShowSucceed _ ->
      model ! []


    AdminFail err ->
      let _ = Debug.log "" err in
      model ! []


    Refresh ->
      { model | refreshing = True, errmsg = "" }
        ! [ refreshApp token "/api/v1/todos/refresh" model ]

    RefreshSucceed refreshModel ->
      case refreshModel.refresh of
        "ok" ->
          { model
          | refreshing = False
          , todos = Valid refreshModel.todos
          } ! []

        _ ->
          { model | refreshing = False, errmsg = "更新失败，未能获取到待办数据" } ! []

    RefreshFail err ->
      let
        _ = Debug.log "refresh failure: " err
      in
        { model | refreshing = False, errmsg = "更新失败，请咨询信息管理员" } ! []





-- View


view : Model -> Html Msg
view model =
  div [ class "todo application" ]
      [ div [ class "header" ]
            [ i [ class "fa fa-th-large" ] []
            , a [
                  href model.homepage, target "_blank"
                ]
                [
                  text ("  " ++ model.app ++ todosCounter(model.todos))
                , i [ class "fa fa-external-link" ] []
                ]
            , span [ class "admin right" ]
                [
                  (refreshWidget model)
                , (errorWidget model)
                ]
            , span [ class "right" ]
                [ text ("(最后更新时间：" ++ model.updateAt ++ ")") ]
            ]
      , (widget model)
      ]


refreshWidget : Model -> Html Msg
refreshWidget model =
  case model.refreshing of
    True ->
      a []
        [ i [ class "fa fa-refresh fa-spin fa-1x fa-fw", title "正在更新..." ] [] ]

    False ->
      a [ onClick Refresh ]
        [ i [ class "fa fa-refresh", title "即时更新" ] [] ]


errorWidget : Model -> Html Msg
errorWidget model =
  case model.errmsg of
    "" ->
      span [] []

    errmsg' ->
      span [ class "error" ] [ text errmsg' ]



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
  let _ = Debug.log "...." model in

  case model.repo of
    Nothing ->
      case model.todos of
        Valid todos' ->
          div [ class "content" ]
              [ itemsContainer todos' model.app ]
        Invalid error' ->
          div [ class "error content" ]
              [ text "系统暂未获取到应用数据，请耐心等待，或者咨询系统管理员，或者"
              , br [] []
              , p []
                  [ span
                      [ class "button-warning pure-button button-xsmall", onClick Register ]
                      [ text "重新授权" ]
                  ]
              ]

    Just repo ->
      case model.registering of
        False ->
          registerReqWidget

        True ->
          div [ class "info content" ]
              [ regForm model.repo ]


registerReqWidget : Html Msg
registerReqWidget =
  div [ class "warning content" ]
      [
        p []
          [
            span [] [ text "您还没有授权获取该应用的待办，需要您进行授权登记。如需获取待办，请您" ]
          ]
      , p []
          [
            a [ class "button-warning pure-button button-xsmall", onClick Register ]
              [ text "授权登记" ]
          ]
      ]



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
      span [ class ("flag-" ++ item.flag) ]
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

refreshApp : Token -> String -> Model -> Cmd Msg
refreshApp token url model =
  case token of
    Nothing ->
      Cmd.none

    Just token' ->
      let
        body' =
          E.object
            [ ( "service_id", E.int model.appId ) ]
          |> E.encode 0
          |> Http.string
      in
        Task.perform RefreshFail RefreshSucceed
          ( Jwt.post token' decodeRefreshModel url body' )


decodeRefreshModel : Json.Decoder RefreshModel
decodeRefreshModel =
  Json.object3 RefreshModel
    ("updated_at" := int)
    ("todos" := decodeTodos)
    ("refresh" := string)


decodeTodos : Json.Decoder (List Item)
decodeTodos =
  let
    item =
      Json.object4 Item
        ("message" := string)
        ("url" := string)
        ("update_at" := string)
        ("flag" := string)
  in
    Json.list item





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
            Task.perform RegFail RegSucceed (Jwt.post token' (Json.succeed "ok") url body')


decodeRepo : Json.Decoder (Maybe Repo)
decodeRepo =
  Json.maybe
    <| Json.object2 Repo
        ("id" := int)
        ("name" := string)

