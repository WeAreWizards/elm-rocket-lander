module View exposing (..)

import Html exposing (..)
import Color exposing (Color, rgba, rgb, black)
import String
import Graphics.Render as Render exposing (..)
import Models exposing (..)
import Messages exposing (..)
import Update exposing (..)


paint : Model -> Html Msg
paint model =
    let
        ( w_, h_ ) =
            ( model.width - 10, model.height - 10 )

        ( w, h ) =
            ( toFloat w_, toFloat h_ )
    in
        case model.state of
            PreRunning ->
                Render.svg 0
                    0
                    w
                    h
                    (Render.group
                        [ paintGame model ( w_, h_ )
                        , startScreen ( w_, h_ )
                        ]
                    )

            Running ->
                Render.svg 0
                    0
                    w
                    h
                    (Render.group
                        [ paintGame model ( w_, h_ ) ]
                    )

            Lost ->
                Render.svg 0
                    0
                    w
                    h
                    (Render.group
                        [ paintGame model ( w_, h_ )
                        , lostScreen ( w_, h_ )
                        ]
                    )

            Won ->
                Render.svg 0
                    0
                    w
                    h
                    (Render.group
                        [ paintGame model ( w_, h_ )
                        , wonScreen ( w_, h_ )
                        ]
                    )


paintGame : Model -> ( Int, Int ) -> Render.Form Msg
paintGame model ( w, h ) =
    let
        fw =
            toFloat w

        fh =
            toFloat h
    in
        group
            [ rectangle fw fh |> filled (solid <| black)  |> position ( fw / 2, fh / 2 )
            , paintLandscape generateLandscape ( w, h )
            , paintPlatform model.platformPos ( w, h )
            , paintShip model.ship ( w, h )
            , statsScreen (w, h) model |> position ( fw - 100, 50 )
            ]


paintShip : Ship -> ( Int, Int ) -> Render.Form Msg
paintShip ship ( w, h ) =
    let
        color =
            rgba 255 0 0 255

        screenCoords =
            (toScreenCoords ( w, h ) ( ship.x, ship.y ))
    in
        polygon [ ( 0, 0 ), ( 10, -10 ), ( 10, -20 ), ( -10, -20 ), ( -10, -10 ) ]
            |> filled (solid <| color)
            |> position ( -10, 0 )
            |> position screenCoords
            |> position ( 0, -20 )
            |> angle (ship.roll - pi)
            |> position screenCoords


paintLandscape : ( Float, List ( Float, Float ) ) -> ( Int, Int ) -> Render.Form Msg
paintLandscape ( platformPos, heights ) ( w, h ) =
    let
        closedLoop =
            heights ++ [ ( 0.0, 1.0 ), ( 1.0, 1.0 ) ]
    in
        polygon (List.map (toScreenCoords ( w, h )) closedLoop)
            |> filled (solid <| (rgba 0 255 0 255))


paintPlatform : Float -> ( Int, Int ) -> Render.Form Msg
paintPlatform platformPos ( w, h ) =
    let
        xpos =
            (toFloat w) * (platformPos)

        ypos =
            (toFloat h ) - 1
    in
        rectangle 80 10
            |> filled (solid <| (rgba 255 0 0 255))
            |> position ( xpos, ypos )


genericScreen : ( Int, Int ) -> Color -> String -> Render.Form Msg
genericScreen ( windowSizeX, windowSizeY ) borderColor heading =
    let
        width =
            440

        height =
            150

        width_offset =
            (width / 2)

        height_offset =
            (height / 2)

        xPos =
            (windowSizeX |> toFloat) / 2

        yPos =
            (windowSizeY |> toFloat) / 2
    in
        group
            [ filledAndBordered (solid <| rgb 255 255 255) 2 (solid <| borderColor) (rectangle (width * 2) (height * 2))
            , drawText heading 20 ( 0, -30 ) (rgba 0 0 0 255)
            , drawText "Land gently on the red platform before running out of fuel." 20 ( 0, 0 ) (rgba 0 0 0 255)
            , drawText "Use < and > to roll the ship, ^ to boost." 20 ( 0, 18 ) (rgba 0 0 0 255)
            , drawText "Press SPACE to start" 20 ( 0, 36 ) (rgba 0 0 0 255)
            ]
            |> position ( xPos, yPos )


drawText : String -> Int -> Point -> Color -> Form msg
drawText textContent textSize pos color =
    Render.text textSize textContent
        |> fontColor color
        |> fontFamily "monospace"
        |> centered
        |> position pos


startScreen : ( Int, Int ) -> Render.Form Msg
startScreen windowSize =
    genericScreen windowSize (rgba 255 0 0 255) "Rocket lander in Elm."


lostScreen : ( Int, Int ) -> Render.Form Msg
lostScreen windowSize =
    genericScreen windowSize (Color.rgba 170 0 0 255) "Ouch!"


wonScreen : ( Int, Int ) -> Render.Form Msg
wonScreen windowSize =
    genericScreen windowSize (Color.rgba 0 170 0 255) "Good job, commander."


statsScreen : ( Int, Int ) -> Model -> Render.Form Msg
statsScreen ( w, h ) model =
    let
        fuelColor =
            rgba 255 255 255 255

        speedColor =
            if shipSpeed model.ship < speedCutoff then
                (rgba 0 170 0 255)
            else
                (rgba 255 0 0 255)

        rollColor =
            if abs (model.ship.roll) < rollCutoff then
                (rgba 0 170 0 255)
            else
                (rgba 255 0 0 255)
    in
        group
            [ drawText ("Fuel: " ++ toString model.ship.fuel) 12 ( 0, 0 ) fuelColor
            , drawText ("Speed: " ++ String.slice 0 6 (toString (shipSpeed model.ship))) 12 ( 0, 18 ) speedColor
            , drawText ("Roll: " ++ String.slice 0 6 (toString (model.ship.roll))) 12 ( 0, 36 ) rollColor
            ]
