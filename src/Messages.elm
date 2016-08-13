module Messages exposing (..)

import Keyboard exposing (KeyCode)
import Time exposing (Time)

type Msg
    = TimeUpdate Time
    | KeyDown KeyCode
    | KeyUp KeyCode
    | Resize Int Int
    | NoOp
