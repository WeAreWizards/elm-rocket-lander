import Html.App as Html
import Color
import AnimationFrame
import Window
import Keyboard exposing (KeyCode)
import Html exposing (..)
import Html.Attributes exposing (..)
import String
import Time exposing (Time)
import Random
import List
import Key exposing (..)
import Graphics.Render as Render
import Task

type alias Radians = Float

type GameState = PreRunning | Running | Won | Lost

type alias KeyArrows = 
  { x: Float
  , y: Bool
  }

type alias Ship =
  { x: Float
  , y: Float
  , vx: Float
  , vy: Float
  , roll: Radians
  , boosting: Bool
  , fuel: Float
  , controls: KeyArrows
  }

type alias Model =
  { gravity: Float
  , ship: Ship
  , state: GameState
  , platformPos: Float
  , height: Int
  , width: Int
  }

makeGame : Model
makeGame = { gravity = 0.000078
           , ship = (Ship 0.9 0.4 0 0 0 False 1000 {x=0, y=False})
           , state = PreRunning
           , platformPos = 0.5
           , height = 800
           , width = 1600
           }

speedCutoff : Float
speedCutoff = 0.002

rollCutoff : Float
rollCutoff = 0.19



-------------------------------------------------------------------
main =  
  Html.program
    { init = init
    , update = update
    , view = paint
    , subscriptions = subscriptions
    }
-------------------------------------------------------------------

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs TimeUpdate
        , Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        , Window.resizes (\{width, height} -> Resize width height)
        ]

init : ( Model, Cmd Msg )
init =
    ( makeGame, initialSizeCmd )

initialSizeCmd : Cmd Msg
initialSizeCmd =
  Task.perform (\_ -> NoOp) sizeToMsg Window.size

sizeToMsg : Window.Size -> Msg
sizeToMsg size =
  Resize size.width size.height

type Msg
    = TimeUpdate Time
    | KeyDown KeyCode
    | KeyUp KeyCode
    | Resize Int Int
    | NoOp

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case model.state of
    PreRunning -> case msg of
          KeyDown keyCode -> ( keyDownPreRunning keyCode model, Cmd.none )
          Resize w h      -> ({model | height = h, width = w} , Cmd.none)
          _               -> (model, Cmd.none) 
    Running -> case msg of
          TimeUpdate dt   -> ( updateRunning model, Cmd.none )
          KeyDown keyCode -> ( keyDownRunning keyCode model, Cmd.none )
          KeyUp keyCode   -> ( keyUpRunning keyCode model, Cmd.none )            
          Resize w h      -> ({model | height = h, width = w} , Cmd.none)
          NoOp -> (model, Cmd.none)      
    Lost -> case msg of
          KeyDown keyCode -> ( keyDownIdle keyCode model, initialSizeCmd )
          Resize w h      -> ({model | height = h, width = w} , Cmd.none)
          _               -> (model, Cmd.none) 
    Won -> case msg of
          KeyDown keyCode -> ( keyDownIdle keyCode model, initialSizeCmd )
          Resize w h      -> ({model | height = h, width = w} , Cmd.none)
          _               -> (model, Cmd.none) 


keyDownPreRunning : KeyCode -> Model -> Model
keyDownPreRunning keyCode model =
    case Key.fromCode keyCode of
        Space ->
            { model | state = Running }
        _ ->
            model


keyDownIdle : KeyCode -> Model -> Model
keyDownIdle keyCode model =
    case Key.fromCode keyCode of
        Space ->
            { makeGame | state = Running }
        _ ->
            model

keyDownRunning : KeyCode -> Model -> Model
keyDownRunning keyCode model =
    case Key.fromCode keyCode of
        ArrowLeft ->
            let 
              controls = model.ship.controls
              newControls = {controls | x = -1.0}
              ship = model.ship
              newShip =  {ship | controls = newControls}
            in
              { model | ship = newShip }
        ArrowRight ->
            let 
              controls = model.ship.controls
              newControls = {controls | x = 1.0}
              ship = model.ship
              newShip =  {ship | controls = newControls}
            in
              { model | ship = newShip }
        ArrowUp ->
            let 
              controls = model.ship.controls
              newControls = {controls | y = True}
              ship = model.ship
              newShip =  {ship | controls = newControls}
            in
              { model | ship = newShip }
        _ ->
            model

keyUpRunning : KeyCode -> Model -> Model
keyUpRunning keyCode model =
    case Key.fromCode keyCode of
        ArrowLeft ->
            let 
              controls = model.ship.controls
              newControls = {controls | x = 0.0}
              ship = model.ship
              newShip =  {ship | controls = newControls}
            in
              { model | ship = newShip }
        ArrowRight ->
            let 
              controls = model.ship.controls
              newControls = {controls | x = 0.0}
              ship = model.ship
              newShip =  {ship | controls = newControls}
            in
              { model | ship = newShip }
        ArrowUp ->
            let 
              controls = model.ship.controls
              newControls = {controls | y = False}
              ship = model.ship
              newShip =  {ship | controls = newControls}
            in
              { model | ship = newShip }
        _ ->
            model

updateRunning : Model -> Model
updateRunning model =
  let
    controls = model.ship.controls
    ship = shipUpdate
           model.gravity
           (if model.ship.fuel > 0 then controls.y else False)
           (if model.ship.fuel > 0 then round controls.x else 0)
           model.ship
    (platformPos, landscape) = generateLandscape
  in
    case (isShipAlive (ship.x, ship.y) landscape, isShipLanded ship platformPos) of
      (True, False)  -> {model | ship = ship}
      (True, True)   -> {model | state = Won}
      (False, True)  -> {model | state = Lost}
      (False, False) -> {model | state = Lost}



shipUpdate : Float -> Bool -> Int -> Ship -> Ship
shipUpdate g boosting roll ship =
  let
    accell = { x = if boosting then -3.0 * g * sin(ship.roll) else 0
             , y = if boosting then 3.0 * g * cos(ship.roll) else 0
             }
    fuel' = if boosting then ship.fuel - 3 else ship.fuel
    fuel'' = if abs roll > 0 then fuel' - 1 else fuel'
  in
    { ship | vy = ship.vy + g - accell.y
           , vx = ship.vx - accell.x
           , y = ship.y + ship.vy
           , x = ship.x + ship.vx
           , boosting = boosting
           , fuel = fuel''
           , roll = ship.roll + ( (toFloat roll) / 20.0)
           }

shipSpeed : Ship -> Float
shipSpeed ship = clamp 0.0 1.0 (sqrt (ship.vx * ship.vx + ship.vy * ship.vy))

isShipLanded : Ship -> Float -> Bool
isShipLanded ship platformPos =
  abs ship.y > 0.99
    && abs (ship.x - platformPos) < 0.1
    && shipSpeed ship < speedCutoff
    && abs ship.roll < rollCutoff

{- check whether the ship is below a list of half-planes -}
isShipAlive : (Float, Float) -> List (Float, Float) -> Bool
isShipAlive (sx, sy) landscape =
  let hits = List.foldl (isHit (sx, sy)) (False, (1, 1)) landscape
  in
    fst hits

isHit : (Float, Float) -> (Float, Float) -> (Bool, (Float, Float)) ->  (Bool, (Float, Float))
isHit (shipX, shipY) (newX, newY) (hitSoFar, (prevX, prevY)) =
  let nx = newY - prevY
      ny = prevX - newX  {- normal vector -}
      sx = shipX - prevX
      sy = shipY - prevY  {- urg give me some vector maths .. -}
  in
    {- half plane intersection -}
    (hitSoFar || 
    (nx * sx + ny * sy < 0   
    && shipX < prevX 
    && shipX > newX), 
    (newX, newY))

toScreenCoords : (Int, Int) -> (Float, Float) -> (Float, Float)
toScreenCoords (w, h) (x, y)=
  ((x - 0.5) * (toFloat w), (y - 0.5) * (toFloat h))

------------------------------------
-- VIEW
------------------------------------

paint : Model -> Html Msg
paint model =
  let 
    (w', h') = (model.width - 10, model.height - 10)
    (w, h) = (toFloat w', toFloat h')  in
    case model.state of
      PreRunning -> Render.svg w h 
        (Render.group
        [ paintGame model (w', h')
        , Render.move -200 -100 <| Render.html startScreen
        ])
      Running -> Render.svg w h 
        (Render.group
        [ paintGame model (w', h')])
      Lost -> Render.svg w h
        (Render.group
        [ paintGame model (w', h')
        , Render.move -200 -100 <| Render.html lostScreen
        ])
      Won -> Render.svg w h
        (Render.group
        [ paintGame model (w', h')
        , Render.move -200 -100 <| Render.html wonScreen
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
  , Render.move (fw/2 - 200) -(toFloat h / 2) <| Render.html <| (statsScreen (w, h) model)
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



genericScreen : String -> String -> Html Msg
genericScreen borderColor heading = 
  div [ Html.Attributes.style [("background", "#eee")
              , ("box-shadow", "5px 5px 0px 0px #888")
              , ("padding", "10px")
              , ("width", "400px")
              , ("border", "2px solid")
              , ("border-color", borderColor)
              ]]
  [ h2 [Html.Attributes.style [("margin", "0")]] [Html.text heading]
  , p [] [Html.text "Land gently on the red platform before running out of fuel."]
  , p [] [Html.text "Use < and > to roll the ship, ^ to boost."]
  , p [] [Html.text "Press SPACE to start."]
  ]

startScreen: Html Msg
startScreen = genericScreen "#eee" "Rocket lander in Elm."
lostScreen: Html Msg
lostScreen = genericScreen "#a00" "Ouch!"
wonScreen: Html Msg
wonScreen = genericScreen "#0a0" "Good job, commander."

statsScreen : (Int, Int) -> Model -> Html Msg
statsScreen (w, h) model = 
  let
    speedColor = ("color", if shipSpeed model.ship < speedCutoff then "#0a0" else "#f00")
    rollColor = ("color", if abs(model.ship.roll) < rollCutoff then "#0a0" else "#f00")
  in
  div [Html.Attributes.style [("font-family", "monospace"), ("color", "#fff"), ("width", "200px")]]
  [ p [] [ Html.text ("Fuel: " ++ toString model.ship.fuel) ]
  , p [Html.Attributes.style [speedColor]] [ Html.text ("Speed: " ++ String.slice 0 6 (toString (shipSpeed model.ship))) ]
  , p [Html.Attributes.style [rollColor]] [ Html.text ("Roll: " ++ String.slice 0 6 (toString (model.ship.roll))) ]
  ]


{- Make a landscape with a platform position and some mountain-looking
   things -}
generateLandscape : (Float, List (Float, Float))
generateLandscape =
  let
    platformIndex = Random.int 0 2
    mainValues = [Random.float 0.1 0.2, Random.float 0.4 0.5, Random.float 0.7 0.9]
  in
   (0.5, [(1.0, 0.8), (0.8, 0.6), (0.7, 0.4), (0.6, 0.9), (0.52, 1.0), (0.5, 1.0), (0.48, 1.0), (0.4, 0.8), (0.2, 0.4), (0.0, 0.3)])
