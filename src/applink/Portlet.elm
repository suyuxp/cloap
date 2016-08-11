module Applink.Portlet exposing (..)


import Html exposing (..)
import Html.Attributes exposing (..)
--import Html.Events exposing (..)

import Task
import Http
import Json.Decode as Json exposing ((:=))

import Jwt
import LocalStorage




-- model


type alias Token =
  Maybe LocalStorage.Value


type alias Url =
  String


type alias Applink =
  { id: Int
  , name: String
  , url: String
  , priority: Int
  }


type alias Applinks =
  { public: List Applink
  , private: List Applink
  }


type alias Model =
  { applinks: Applinks
  , fetching: Bool
  , ready: Bool
  , errmsg: String
  }


title : String
title =
  "常用链接"


sortByPriority : List Applink -> List Applink
sortByPriority links =
  List.reverse <| List.sortBy .priority links




init : Model
init =
  { applinks = (Applinks [] [])
  , fetching = False
  , ready = False
  , errmsg = ""
  }




-- update


type Msg
  = Fetch
  | FetchSucceed Applinks
  | FetchFail Http.Error


update : Url -> Token -> Msg -> Model -> (Model, Cmd Msg)
update url token msg model =
  case msg of
    Fetch ->
      case token of
        Nothing ->
          { model | errmsg = "invalid token" } ! []

        Just token' ->
          { model | fetching = True, ready = False }
          !
          [ Task.perform FetchFail FetchSucceed
              ( Jwt.getWithJwt token' decodeApplinks url ) ]


    FetchSucceed applinks ->
      { model | applinks = applinks, fetching = False, ready = True }
      ! []


    FetchFail err ->
      { model | errmsg = (toString err), fetching = False, ready = False }
      ! []




-- view


view : Model -> Html Msg
view model =
  div []
    [ h3 [] [ text title ]
    , div [ class "pure-g" ]
        (List.map viewLinks <| sortByPriority model.applinks.public)
    ]


viewLinks : Applink -> Html Msg
viewLinks link =
  div [ class "applink-item pure-u-1-2" ]
    [
      div []
        [ a [ href link.url, target "_blank" ]
            [ text link.name
            , i [ class "fa fa-external-link suffix" ] []
            ]
        ]
    ]




-- subs


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none




-- http

decodeApplinks : Json.Decoder Applinks
decodeApplinks =
  let
    link' =
      Json.object4 Applink
        ("id" := Json.int)
        ("name" := Json.string)
        ("url" := Json.string)
        ("priority" := Json.int)

    links' =
      Json.list link'

  in
    Json.object2 Applinks
      ("public" := links')
      ("private" := links')








