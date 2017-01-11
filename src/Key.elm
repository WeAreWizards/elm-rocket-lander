module Key exposing (..)


type Key
    = Space
    | ArrowLeft
    | ArrowRight
    | ArrowUp
    | Unknown


fromCode : Int -> Key
fromCode keyCode =
    case keyCode of
        32 ->
            Space

        37 ->
            ArrowLeft

        38 ->
            ArrowUp

        39 ->
            ArrowRight

        72 ->
            ArrowLeft

        75 ->
            ArrowUp

        76 ->
            ArrowRight

        _ ->
            Unknown
