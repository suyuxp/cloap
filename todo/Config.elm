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
  { already: List Service
  , pending: List Service
  }


type alias Model =
  { data: Data
  , show: Bool
  }


type PriorityWay
  = Up
  | Down



init : ( Model, Cmd Msg )
init = ( Model (Data [] []) False
       , Cmd.none
       )


toUserService : Maybe UserService -> UserService
toUserService =
  withDefault (UserService 0 0 0)



-- Update


type Msg
    = Fetch
    | FetchSucceed Data
    | Delete (Maybe UserService)
    | DeleteSucceed Int
    | Add Int
    | AddSucceed UserService
    | PriorityAdjust Int PriorityWay
    | PrioritySucceed UserService
    | HttpFail Http.Error
    | Hide


update : Token -> Msg -> String -> Model -> (Model, Cmd Msg)
update token msg url model =
  case msg of
    Fetch ->
      { model | show = True }
        ! [ getServices token (url ++ "/own") ]

    FetchSucceed data ->
      { model | data = data }
        ! []

    Add servId ->
      model
      ! [ Task.perform HttpFail AddSucceed
            ( Jwt.post
                (withDefault "" token)
                decodeUserService
                url
                (E.object [("service_id", E.int servId)] |> E.encode 0 |> Http.string)
            )
        ]

    AddSucceed serv ->
      { model | data = ( addService model.data serv) }
      ! []

    PriorityAdjust userServId way ->
      let
        way' =
          case way of
            Up -> "up"
            Down -> "down"
      in
        model
        ! [ Task.perform HttpFail PrioritySucceed
              ( Jwt.send
                  "PUT"
                  (withDefault "" token)
                  decodeUserService
                  (url ++ "/" ++ (toString userServId) ++ "/" ++ way')
                  Http.empty
              )
          ]

    PrioritySucceed userServ ->
      { model | data = ( changePriority model.data userServ) }
      ! []

    Delete userService ->
      case userService of
        Nothing ->
          model ! []

        Just userService' ->
          model
          ! [ Task.perform HttpFail DeleteSucceed
                ( Jwt.send
                    "DELETE"
                    (withDefault "" token)
                    (Json.succeed userService'.serviceId)
                    (url ++ "/" ++ (toString userService'.id))
                    Http.empty )
            ]

    DeleteSucceed servId ->
      { model | data = ( removeService model.data servId) }
      ! []

    HttpFail err ->
      model ! []

    Hide ->
      { model | show = False } ! []


removeService : Data -> Int -> Data
removeService data servId =
  let
    already' =
      List.filter (\el -> el.id /= servId) data.already

    service' =
      withDefault (Service 0 "" Nothing)
      <| List.head
      <| List.filter (\el -> el.id == servId) data.already

    pending' =
      (Service servId service'.name Nothing)
      :: data.pending
  in
    { data | already = already', pending = pending' }


addService : Data -> UserService -> Data
addService data userServ =
  let
    pending' =
      List.filter (\el -> el.id /= userServ.serviceId) data.pending

    service' =
      withDefault (Service 0 "" Nothing)
      <| List.head
      <| List.filter (\el -> el.id == userServ.serviceId) data.pending

    already' =
      (Service userServ.serviceId service'.name (Just userServ))
      :: data.already
  in
    { data | already = already', pending = pending' }


changePriority : Data -> UserService -> Data
changePriority data userServ =
  let
    already' =
      List.filter (\el -> el.id /= userServ.serviceId) data.already

    service' =
      withDefault (Service 0 "" Nothing)
      <| List.head
      <| List.filter (\el -> el.id == userServ.serviceId) data.already

    service'' =
      { service' | userService = (Just userServ) }
  in
    { data | already = (service'' :: already') }






-- View


view : Model -> Html Msg
view model =
  if (not model.show) then
    div [] []
  else
    div
      []
      [ if (List.isEmpty model.data.already) then
          div
            []
            [ div
                [ class "alert" ]
                [ text "目前您没有选择跟踪任何应用 ！" ]
            , hr [] []
            ]
        else
          appList "您目前选中的应用：" model.data.already alreadyItem
      , if (List.isEmpty model.data.pending) then
          div [] []
        else
          appList "以下应用没有跟踪待办：" model.data.pending pendingItem
      ]



header : String -> Html Msg
header title =
  div
    [ class "header" ]
    [ text title ]


appList : String -> List Service -> ((Int, Service) -> Html Msg) -> Html Msg
appList title services item =
  let
    services' =
      List.sortBy (\el -> .priority <| toUserService el.userService) services
  in
    div
      []
      [ div
          [ class "header" ]
          [ text title ]
      , p []
          ( List.map item
            <| List.indexedMap (,) services'
          )
      ]


alreadyItem : (Int, Service) -> Html Msg
alreadyItem (index, item) =
  let
    userServ' =
      toUserService item.userService
  in

  div
    [ class "service" ]
    [ div [] [ text ((toString (index + 1)) ++ ". " ++ item.name) ]
    , div
        []
        [ text "("
        , a [ onClick (Delete item.userService), title "取消跟踪" ]
            [ i [ class "fa fa-minus-square" ] [] ]
        , a [ onClick (PriorityAdjust userServ'.id Up), title "上移" ]
            [ i [ class "fa fa-arrow-circle-up" ] [] ]
        , a [ onClick (PriorityAdjust userServ'.id Down), title "下移" ]
            [ i [ class "fa fa-arrow-circle-down" ] [] ]
        , text ")"
        ]
    ]


pendingItem : (Int, Service) -> Html Msg
pendingItem (index, item) =
  div
    [ class "service" ]
    [ div [] [ text ((toString (index + 1)) ++ ". " ++ item.name) ]
    , div
        []
        [ text "("
        , a [ onClick (Add item.id), title "跟踪应用" ]
            [ i [ class "fa fa-plus-square" ] [] ]
        , text ")"
        ]
    ]




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
