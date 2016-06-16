module Todo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as HtmlApp
import Html.Lazy exposing (..)

import Http
import Json.Decode as Json exposing ( (:=), Value, string, int, list, maybe, object2, object3 )
import Json.Encode as E

import Platform.Cmd exposing (Cmd)
import Task exposing (toResult)
import Result exposing (Result(Ok,Err))

import Jwt exposing (..)
import LocalStorage

import Todo.App as TodoAppWidget




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
    , errmsg: String
    }


init : ( Model, Cmd Msg )
init = ( Model False [] ""
       , Cmd.none
       )




-- UPDATE


type Msg
  = Fetch
  | FetchSucceed (List AppTodo)
  | FetchFail Http.Error
  | SubMsg String TodoAppWidget.Msg


update : Token -> Msg -> Model -> (Model, Cmd Msg)
update token msg model =
  case msg of
    Fetch ->
      { model | ready = False }
        ! [ getTodos token model ]

    FetchSucceed val ->
      ( { model | ready = True, todos = val }
      , Cmd.none
      )

    FetchFail err ->
      ( { model | errmsg = toString err }, Cmd.none )

    SubMsg app subMsg ->
      let
        (newTodos, cmds) =
          List.unzip (List.map (updateHelp token app subMsg) model.todos)
      in
        ( { model | todos = newTodos }
        , Cmd.batch cmds
        )


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
  Sub.batch (List.map subHelp model.todos)


subHelp : AppTodo -> Sub Msg
subHelp {id, model} =
  Sub.map (SubMsg id) (TodoAppWidget.subscriptions model)




-- VIEW

view : Model -> Html Msg
view model =
  div [ class "content-container" ]
      [
        h3 []
          [ i [ class "fa fa-first-order fa-lg" ] []
          , span [] [ text "我的工作" ]
          ]
      , div []
            [ lazy todosView model.todos ]
      ]


todosView : List AppTodo -> Html Msg
todosView todos =
  div [ class "pure-g" ]
      (List.map todoWidget todos)


todoWidget : AppTodo -> Html Msg
todoWidget appTodo =
  div [ class "pure-u-1-2" ]
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
      object3 convAppTodo
        ("app" := string)
        ("todos" :=
          ( Json.oneOf
              [ Json.map TodoAppWidget.Valid decodeTodo,
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


convAppTodo : String -> TodoAppWidget.Todos -> Maybe TodoAppWidget.Repo -> AppTodo
convAppTodo app todos repo =
  AppTodo app (TodoAppWidget.init app todos repo |> fst)


decodeTodo : Json.Decoder (List TodoAppWidget.Item)
decodeTodo =
  let
    item =
      object3 TodoAppWidget.Item
        ("message" := string)
        ("url" := string)
        ("update_at" := string)
  in
    Json.list item


decodeInvalidContent : Json.Decoder TodoAppWidget.InvalidContent
decodeInvalidContent =
  object2 TodoAppWidget.InvalidContent
    ("result" := string)
    ("reason" := string)