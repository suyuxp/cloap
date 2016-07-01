module Todo.Config exposing (..)


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


type alias Service =
  { id: Int
  , name: String
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
    | FetchFail Http.Error


update : Token -> Msg -> String -> Model -> (Model, Cmd Msg)
update token msg url model =
  case msg of
    Fetch ->
        { model | show = True }
        ! [ getServices token url ]

    FetchSucceed data ->
        { model | data = data } ! []

    FetchFail err ->
        model ! []





-- View


view : Model -> Html Msg
view model =
  if (not model.show) then
    div [] []
  else
    div
      []
      [ appList "您选中的应用：" model.data.aleady
      , appList "以下应用没有跟踪待办：" model.data.pending
      ]



appList : String -> List Service -> Html Msg
appList title services =
  div
    []
    [
      div
        [ class "header" ]
        [ text title ]
    , p []
        (List.map appItem (List.indexedMap (,) services))
    ]

appItem : (Int, Service) -> Html Msg
appItem (index, item) =
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
            Task.perform FetchFail FetchSucceed
                ( Jwt.getWithJwt token' decodeModel url )


decodeModel: Json.Decoder Data
decodeModel =
  Json.object2 Data
    ("aleady" := decodeServices)
    ("pending" := decodeServices)


decodeServices : Json.Decoder (List Service)
decodeServices =
    let
        servDecoder =
            Json.object2 Service
                ("id" := int)
                ("name" := string)
    in
        Json.list servDecoder
