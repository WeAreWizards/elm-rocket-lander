module View exposing (..)

import Html exposing (..)
import Color
import String
import Graphics.Render as Render

import Models exposing (..)
import Messages exposing (..)
import Update exposing (..)


paint : Model -> Html Msg
paint model =
  let 
    (w', h') = (model.width - 10, model.height - 10)
    (w, h) = (toFloat w', toFloat h')  in
    case model.state of
      PreRunning -> Render.svg w h 
        (Render.group
        [ paintGame model (w', h')
          , startScreen  
        ])
      Running -> Render.svg w h 
        (Render.group
        [ paintGame model (w', h')])
      Lost -> Render.svg w h
        (Render.group
        [ paintGame model (w', h')
        , lostScreen
        ])
      Won -> Render.svg w h
        (Render.group
        [ paintGame model (w', h')
        , wonScreen
        ])


paintGame : Model -> (Int, Int) -> Render.Form Msg
paintGame model (w, h) =
  let fw = toFloat w
      fh = toFloat h
  in
  Render.group
  [ Render.solidFill Color.black(Render.rectangle fw fh)
  , paintLandscape generateLandscape (w, h)
  , paintPlatform model.platformPos (w, h)
  , paintShip model.ship (w, h)
  , Render.move (fw/2 - 200) -(toFloat h / 2 - 20) <| (statsScreen (w, h) model)
  ]

paintShip : Ship -> (Int, Int) -> Render.Form Msg
paintShip ship (w, h) =
  let
    color = Color.rgba 255 0 0 255
    screenCoords = (toScreenCoords (w, h) (ship.x, ship.y))
  in
  Render.rotate (ship.roll) <|
  Render.rotate (pi) <|
  Render.move 0 -20 <| {- Paint a bit higher so it looks like the ship dies on contact, not when deep in the land -}
  Render.move (fst screenCoords) (snd screenCoords) <|
  Render.move -10 0 <|
  Render.solidFill color (Render.polygon [(0, 0), (10, -10), (10, -20), (-10, -20), (-10, -10)])

paintLandscape : (Float, List (Float, Float)) -> (Int, Int) -> Render.Form Msg
paintLandscape (platformPos, heights) (w, h) =
  let closedLoop = heights ++ [(0.0, 1.0), (1.0, 1.0)]
  in
  Render.solidFill (Color.rgba 0 255 0 255)
    (Render.polygon (List.map  (toScreenCoords (w, h)) closedLoop))

paintPlatform : Float -> (Int, Int) -> Render.Form Msg
paintPlatform platformPos (w, h) =
  let 
    xpos = (toFloat w) * (platformPos - 0.5)
    ypos = (toFloat h / 2) - 5
  in
    Render.move xpos ypos <|
    Render.solidFill (Color.rgba 255 0 0 255) (Render.rectangle 80 10)

genericScreen : Color.Color -> String -> Render.Form Msg
genericScreen borderColor heading = 
  let
    width = 440
    height = 150
    offset = -( width / 2) + 5 
  in
  Render.group [  Render.solidFillWithBorder (Color.rgba 255 255 255 255) 2 borderColor (Render.rectangle width height)
                , Render.move offset -30 <| Render.bold 20 "monospace" (Color.rgba 0 0 0 255) heading   
                , Render.move offset 0 <| Render.bold 12 "monospace" (Color.rgba 0 0 0 255) "Land gently on the red platform before running out of fuel."
                , Render.move offset 18 <| Render.bold 12 "monospace" (Color.rgba 0 0 0 255) "Use < and > to roll the ship, ^ to boost."
                , Render.move offset 36 <| Render.bold 12 "monospace" (Color.rgba 0 0 0 255) "Press SPACE to start."
  ]

startScreen: Render.Form Msg
startScreen = genericScreen (Color.rgba 255 0 0 255) "Rocket lander in Elm."
lostScreen: Render.Form Msg
lostScreen = genericScreen (Color.rgba 170 0 0 255) "Ouch!"
wonScreen: Render.Form Msg
wonScreen = genericScreen (Color.rgba 0 170 0 255) "Good job, commander."

statsScreen : (Int, Int) -> Model -> Render.Form Msg
statsScreen (w, h) model = 
  let
    fuelColor = Color.rgba 255 255 255 255
    speedColor = if shipSpeed model.ship < speedCutoff then (Color.rgba 0 170 0 255) else (Color.rgba 255 0 0 255)
    rollColor = if abs(model.ship.roll) < rollCutoff then (Color.rgba 0 170 0 255) else (Color.rgba 255 0 0 255)
  in
  Render.group [  Render.move 0 0 <| Render.bold 12 "monospace" fuelColor ("Fuel: " ++ toString model.ship.fuel)
                , Render.move 0 18 <| Render.bold 12 "monospace" speedColor ("Speed: " ++ String.slice 0 6 (toString (shipSpeed model.ship)))
                , Render.move -0 36 <| Render.bold 12 "monospace" rollColor ("Roll: " ++ String.slice 0 6 (toString (model.ship.roll)))
  ]


