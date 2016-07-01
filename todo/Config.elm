module Todo.Config exposing (..)


import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Http
import Task
import Json.Decode as Json exposing ((:=), int, string)
import Json.Encode as E
import Maybe exposing (withDefault)

import Jwt
import LocalStorage




-- Model


type alias Token =
  Maybe LocalStorage.Value


type alias UserService =
  { id: Int
  , priority: Int
  , serviceId: Int
  }


type alias Service =
  { id: Int
  , name: String
  , userService: Maybe UserService
  }


type alias Data =
  { aleady: List Service
  , pending: List Service
  }


type alias Model =
  { data: Data
  , show: Bool
  }


init : ( Model, Cmd Msg )
init = ( Model (Data [] []) False
       , Cmd.none
       )




-- Update


type Msg
    = Fetch
    | FetchSucceed Data
    | Delete (Maybe UserService)
    | DeleteSuccess Int
    | HttpFail Http.Error


update : Token -> Msg -> String -> Model -> (Model, Cmd Msg)
update token msg url model =
  case msg of
    Fetch ->
      { model | show = True }
        ! [ getServices token (url ++ "/own") ]

    FetchSucceed data ->
      { model | data = data }
        ! []

    Delete userService ->
      let
        _ = Debug.log "..." userService
      in

      case userService of
        Nothing ->
          model ! []

        Just userService' ->
          model
          ! [ Task.perform HttpFail DeleteSuccess
                ( Jwt.send
                    "DELETE"
                    (withDefault "" token)
                    (Json.succeed userService'.serviceId)
                    (url ++ "/" ++ (toString userService'.id))
                    Http.empty )
            ]

    DeleteSuccess servId ->
      let
        aleady' =
          List.filter (\el -> el.id == servId) model.data.aleady

        data' =
          model.data

        data'' =
          { data' | aleady = aleady' }
      in
        { model | data = data'' }
          ! []

    HttpFail err ->
      model ! []





-- View


view : Model -> Html Msg
view model =
  if (not model.show) then
    div [] []
  else
    div
      []
      [ appList "您选中的应用：" model.data.aleady alreadyItem
      , appList "以下应用没有跟踪待办：" model.data.pending pendingItem
      ]



header : String -> Html Msg
header title =
  div
    [ class "header" ]
    [ text title ]


appList : String -> List Service -> ((Int, Service) -> Html Msg) -> Html Msg
appList title services item =
  div
    []
    [ div
        [ class "header" ]
        [ text title ]
    , p []
        (List.map item (List.indexedMap (,) services))
    ]


alreadyItem : (Int, Service) -> Html Msg
alreadyItem (index, item) =
  div
    []
    [ text ((toString (index + 1)) ++ ". " ++ item.name)
    , a [ onClick (Delete item.userService)] [ text "移除" ]
    ]


pendingItem : (Int, Service) -> Html Msg
pendingItem (index, item) =
  div
    []
    [ text ((toString (index + 1)) ++ ". " ++ item.name) ]




-- Subs


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none




-- HTTP


getServices : Token -> String -> Cmd Msg
getServices token url =
    case token of
        Nothing ->
            Cmd.none

        Just token' ->
            Task.perform HttpFail FetchSucceed
                ( Jwt.getWithJwt token' decodeModel url )



decodeModel: Json.Decoder Data
decodeModel =
  Json.object2 Data
    ("already" := decodeServices)
    ("pending" := decodeServices)


decodeServices : Json.Decoder (List Service)
decodeServices =
    let
        servDecoder =
            Json.object3 Service
                ("id" := int)
                ("name" := string)
                (Json.maybe ("user_service" := decodeUserService))

    in
        Json.list servDecoder


decodeUserService : Json.Decoder UserService
decodeUserService =
  Json.object3 UserService
    ("id" := int)
    ("priority" := int)
    ("service_id" := int)
