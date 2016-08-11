module Todo.App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as App

import Http
import Task
import Json.Decode as Json exposing ((:=), int, string, bool)
import Json.Encode as E

import Jwt
import LocalStorage

import Common exposing (..)
import Todo.Authorize as Authorize

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



type alias Model =
  { todos: Todos
  ------ Info
  , app: String
  , appId: Int
  , repoId: Int
  , homepage: String
  , updateAt: String
  , links: Links
  , repo: Maybe Repo
  ------ Register Form
  , authorize: Authorize.Model
  ------
  , errmsg : String
  ------ Refresh
  , registering: Bool
  , refreshing: Bool
  }


init : String -> Int -> Int -> String -> String -> Links -> Todos -> Maybe Repo -> Model
init app appId repoId homepage updateAt links todos repo =
  { todos = todos
  , app = app
  , appId = appId
  , repoId = repoId
  , homepage = homepage
  , updateAt = updateAt
  , links = links
  , repo = repo
  , authorize = Authorize.initEmpty
  , errmsg = ""
  , registering = False
  , refreshing = False
  }




-- Update


type Msg
  = Register
  ------ Admin
  | UpPriority Todos
  | DownPriority Todos
  | SetDefaultShowSucceed Int
  | AdminFail Http.Error
  ------ register form
  | AuthorizePage Authorize.Msg
  ------ refresh
  | Refresh
  | RefreshSucceed RefreshModel
  | RefreshFail Http.Error


authorizePath : Url -> Url
authorizePath base =
  base ++ "/" ++ "accounts"


update : Url -> Token -> Msg -> Model -> (Model, Cmd Msg)
update url token msg model =
  --let _ = Debug.log "msg ...." msg in

  case msg of
    Register ->
      { model | registering = True, authorize = (Authorize.init model.repoId) }
      ! []

    AuthorizePage submsg ->
      --let _ = Debug.log "submsg ..." submsg in

      case submsg of
        Authorize.CancelRemote ->
          { model | registering = False } ! []

        Authorize.UpdateSucceed auth ->
          update url token Refresh { model | registering = False }

        _ ->
          let
            (model', cmds') =
              Authorize.update (authorizePath url) token submsg model.authorize
          in
            { model | authorize = model' }
            ! [ Cmd.map AuthorizePage cmds' ]



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
      case model.registering of
        True ->
          div [ class "info content" ]
              [ regForm model ]

        False ->
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
              [ regForm model ]


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


regForm : Model -> Html Msg
regForm model =
  App.map AuthorizePage (Authorize.view model.authorize)





-- Subs


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.map AuthorizePage (Authorize.subscriptions model.authorize)




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


decodeRepo : Json.Decoder (Maybe Repo)
decodeRepo =
  Json.maybe
    <| Json.object2 Repo
        ("id" := int)
        ("name" := string)

