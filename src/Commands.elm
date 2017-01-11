module Commands exposing (..)

import Task
import Messages exposing (..)
import Window


initialSizeCmd : Cmd Msg
initialSizeCmd =
    Task.perform sizeToMsg Window.size


sizeToMsg : Window.Size -> Msg
sizeToMsg size =
    Resize size.width size.height
