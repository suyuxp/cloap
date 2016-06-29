module Todo.UserChoice exposing (..)


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


type alias Model =
    { aleady: List Service
    , pending: List Service
    }


init : ( Model, Cmd Msg )
init = ( Model [] []
       , Cmd.none
       )




-- Update


type Msg
    = Fetch
    | FetchSucceed Model
    | FetchFail Http.Error


update : Token -> Msg -> String -> Model -> (Model, Cmd Msg)
update token msg url model =
  case msg of
    Fetch ->
        model
        ! [ getServices token url ]

    FetchSucceed model' ->
        model' ! []

    FetchFail err' ->
        model ! []




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


decodeModel: Json.Decoder Model
decodeModel =
    Json.object2 Model
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
