module Subscriptions exposing (..)

import AnimationFrame
import Window
import Keyboard exposing (KeyCode)


import Models exposing (..)
import Messages exposing (..)

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs TimeUpdate
        , Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        , Window.resizes (\{width, height} -> Resize width height)
        ]
