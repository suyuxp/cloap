module Todo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as HtmlApp
import Html.Lazy exposing (..)
import Date
import String

import Http
import Json.Decode as Json exposing ( (:=), Value, string, int, list, maybe, object2, object3, object4 )
--import Json.Encode as E

import Platform.Cmd exposing (Cmd)
import Task exposing (toResult)
import Result exposing (Result(Ok,Err))
import Time exposing (Time, inMinutes)

import Jwt exposing (..)
import LocalStorage

import Todo.App as TodoAppWidget
import Todo.Config as Config

import Applink.Portlet as Applink





-- MODEL


type alias Token =
  Maybe LocalStorage.Value


type alias AppTodo =
  { id: String
  , model: TodoAppWidget.Model
  }


type alias Model
  = { ready: Bool
    , todos: List AppTodo
    , config: Config.Model
    , applink: Applink.Model
    , errmsg: String
    }


init : ( Model, Cmd Msg )
init =
  let
    ( configModel',  configCmds' ) =
      Config.init

    model' =
      { ready   = False
      , todos   = []
      , config  = configModel'
      , applink = Applink.init
      , errmsg  = ""
      }
  in
    ( model'
    , Cmd.none
    )




-- UPDATE


type Msg
  = Fetch
  | Tick Time
  | FetchSucceed (List AppTodo)
  | FetchFail Http.Error
  | SubMsg String TodoAppWidget.Msg
  | ConfigWidget Config.Msg
  | ApplinkPortlet Applink.Msg



update : Token -> Msg -> Model -> (Model, Cmd Msg)
update token msg model =
  case msg of
    Fetch ->
      let
        (model', cmd') =
          Applink.update "/api/v1/applinks" token Applink.Fetch model.applink
      in
        ({ model | ready = False }
        , Cmd.batch [ getTodos token model, Cmd.map ApplinkPortlet cmd' ]
        )

    Tick _ ->
      { model | ready = False }
        ! [ getTodos token model ]

    FetchSucceed val ->
      if List.isEmpty val then
        update token (ConfigWidget Config.Fetch) {model | ready = True, todos = val}
      else
        { model | ready = True, todos = val } ! []

    FetchFail err ->
      let _ = Debug.log "" err in
      ( { model | errmsg = toString err }, Cmd.none )

    SubMsg app subMsg ->
      let
        (newTodos, cmds) =
          List.unzip (List.map (updateHelp token app subMsg) model.todos)
      in
        ( { model | todos = newTodos }
        , Cmd.batch cmds
        )

    ConfigWidget msg' ->
      let
        (model', cmds') =
          Config.update token msg' "/api/v1/userServices" model.config
      in
        { model | config = model' }
          ! [ Cmd.map ConfigWidget cmds' ]

    ApplinkPortlet msg' ->
      let
        (model', cmds') =
          Applink.update "/api/v1/applinks" token msg' model.applink
      in
        { model | applink = model' }
          ! [ Cmd.map ApplinkPortlet cmds' ]





updateHelp : Token -> String -> TodoAppWidget.Msg -> AppTodo -> ( AppTodo, Cmd Msg )
updateHelp token id msg appTodo =
  if appTodo.id /= id then
    ( appTodo, Cmd.none )

  else
    let
      ( newAppTodo, cmds ) =
        TodoAppWidget.update token msg appTodo.model
    in
      ( AppTodo id newAppTodo
      , Cmd.map (SubMsg id) cmds
      )






-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    ( List.map subHelp model.todos
      |> (::) ( Time.every (5 * Time.minute) Tick )
      |> (::) ( Sub.map ConfigWidget (Config.subscriptions model.config) )
      |> (::) ( Sub.map ApplinkPortlet (Applink.subscriptions model.applink) )
    )



subHelp : AppTodo -> Sub Msg
subHelp {id, model} =
  Sub.map (SubMsg id) (TodoAppWidget.subscriptions model)




-- VIEW

view : Token -> Model -> Html Msg
view token model =
  div [ class "content-container" ]
      [
        h3 []
          [ i [ class "fa fa-first-order fa-lg" ] []
          , span [] [ text "我的工作" ]
          , if model.config.show then
              closeAdminButton
            else
              a [ onClick (ConfigWidget Config.Fetch), class "right" ]
                [ i [ class "fa fa-cubes" ] []
                , text "应用配置"
                ]
          ]
      , div []
            [
              case token of
                Nothing ->
                  div [ class "alert" ] [ text "待办信息只能登录后才能查阅，请登录后继续。" ]

                Just _ ->
                  div
                    [ class "pure-g" ]
                    [ div
                        [ class "pure-u-3-4" ]
                        [ lazy todosView model.todos ]
                    , if model.config.show then
                        (div
                          [ class "pure-u-1-4 portal-admin right" ]
                          [ div
                              [ class "portal-workarea" ]
                              [ h3 [] [ text "应用配置" ]
                              , HtmlApp.map ConfigWidget (Config.view model.config)
                              , hr [] []
                              , footer [ class "right" ]
                                [ a [ onClick Fetch ]
                                    [ i [ class "fa fa-eye-slash" ] []
                                    , text "查看效果"
                                    ]
                                , closeAdminButton
                                ]
                              , span [ class "clearfix" ] []
                              ]
                          ]
                        )
                      else
                        (div [] [])
                    , (applinkPortlet model.applink)
                    ]
            ]
      ]


applinkPortlet : Applink.Model -> Html Msg
applinkPortlet applink =
  div [ class "pure-u-1-4 portal-admin" ]
    [ div [ class "portal-workarea"]
        [ HtmlApp.map ApplinkPortlet (Applink.view applink) ]
    ]




closeAdminButton : Html Msg
closeAdminButton =
  a [ onClick (ConfigWidget Config.Hide), class "right" ]
    [ i [ class "fa fa-times-circle" ] []
    , text "关闭配置"
    ]


todosView : List AppTodo -> Html Msg
todosView todos =
  div [ class "pure-g" ]
      ( List.map todoWidget todos )


todoWidget : AppTodo -> Html Msg
todoWidget appTodo =
  div [ class "pure-u-1" ]
      [ HtmlApp.map (SubMsg appTodo.id) (TodoAppWidget.view appTodo.model) ]



-- HTTP


getTodos : Token -> Model -> Cmd Msg
getTodos token model =
  case token of
    Nothing ->
      Cmd.none

    Just token' ->
      let
        url = "/api/v1/todos"
      in
        Task.perform FetchFail FetchSucceed
          ( Jwt.getWithJwt token' decodeTodoItems url )


decodeTodoItems : Json.Decoder (List AppTodo)
decodeTodoItems =
  let
    todo =
      Json.object8 convAppTodo
        ("app" := string)
        ("appId" := int)
        ("homepage" := string)
        ("updated_at" := string)
        ("defaultShow" := int)
        ("links" := object2 TodoAppWidget.Links
                      ("priorityUp" := string)
                      ("priorityDown" := string)
        )
        ("todos" :=
          ( Json.oneOf
              [ Json.map TodoAppWidget.Valid decodeTodo,
                Json.map TodoAppWidget.Valid (Json.null []),
                Json.map TodoAppWidget.Invalid decodeInvalidContent
              ]
          )
        )
        (maybe
          ("require"
            := object2 TodoAppWidget.Repo
                ("id" := int)
                ("name" := string)
          )
        )
  in
    Json.list todo



convAppTodo : String -> Int -> String -> String -> Int -> TodoAppWidget.Links -> TodoAppWidget.Todos -> Maybe TodoAppWidget.Repo -> AppTodo
convAppTodo app appId homepage updated_at defaultShow links todos repo =
  let
    update =
      case Date.fromString updated_at of
        Ok date' ->
          dateToString date'

        Err _ ->
          ""
  in
    AppTodo app (TodoAppWidget.init app appId homepage update defaultShow links todos repo |> fst)



dateToString : Date.Date -> String
dateToString date =
  (toString <| Date.year date)
  ++ "-"
  ++ (padDateMonth <| Date.month date)
  ++ "-"
  ++ (padDate <| Date.day date)
  ++ " "
  ++ (padDate <| Date.hour date)
  ++ ":"
  ++ (padDate <| Date.minute date)
  ++ ":"
  ++ (padDate <| Date.second date)


padDate : Int -> String
padDate num =
  String.padLeft 2 '0' <| toString num


padDateMonth : Date.Month -> String
padDateMonth month =
  case month of
    Date.Jan -> "01"
    Date.Feb -> "02"
    Date.Mar -> "03"
    Date.Apr -> "04"
    Date.May -> "05"
    Date.Jun -> "06"
    Date.Jul -> "07"
    Date.Aug -> "08"
    Date.Sep -> "09"
    Date.Oct -> "10"
    Date.Nov -> "11"
    Date.Dec -> "12"



decodeTodo : Json.Decoder (List TodoAppWidget.Item)
decodeTodo =
  let
    item =
      Json.object4 TodoAppWidget.Item
        ("message" := string)
        ("url" := string)
        ("update_at" := string)
        ("flag" := string)
  in
    Json.list item


decodeInvalidContent : Json.Decoder TodoAppWidget.InvalidContent
decodeInvalidContent =
  object2 TodoAppWidget.InvalidContent
    ("result" := string)
    ("reason" := string)