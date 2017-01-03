module Main exposing (..)

import Html
import Models exposing (..)
import Messages exposing (..)
import Update exposing (..)
import Commands exposing (..)
import View exposing (..)
import Subscriptions exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = paint
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    ( makeGame, initialSizeCmd )
