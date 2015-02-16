import Char
import Color
import Graphics.Element (..)
import Graphics.Collage (..)
import Signal
import Window
import Keyboard
import Html (..)
import Html.Attributes (..)
import String
import Time(fps)
import Random
import List


type alias Radians = Float

type GameState = PreRunning | Running | Won | Lost
type KeyInput = ArrowInput {x: Int, y: Int} | SpaceInput Bool


type alias Ship =
  { x: Float
  , y: Float
  , vx: Float
  , vy: Float
  , roll: Radians
  , boosting: Bool
  , fuel: Float
  }

type alias Game =
  { gravity: Float
  , ship: Ship
  , state: GameState
  , platformPos: Float
  }

makeGame : Game
makeGame = { gravity = 0.000078
           , ship = (Ship 0.1 0.6 0 0 0 False 1000)
           , state = PreRunning
           , platformPos = 0.5
           }

speedCutoff : Float
speedCutoff = 0.002

rollCutoff : Float
rollCutoff = 0.19

main : Signal Element
main =
  Signal.map2 paint model Window.dimensions

spacePressed : Signal Bool
spacePressed = Keyboard.isDown (Char.toCode ' ')

keyInput : Signal KeyInput
keyInput = Signal.merge
  (Signal.map ArrowInput Keyboard.arrows)
  (Signal.map SpaceInput spacePressed)

model : Signal Game
model = Signal.foldp update makeGame (Signal.sampleOn (fps 30) keyInput)


update : KeyInput -> Game -> Game
update input game =
  case game.state of
    PreRunning -> case input of
      SpaceInput True -> { game | state <- Running }
      _               -> game
    Running -> case input of
      ArrowInput arrows -> updateRunning arrows game
      _                 -> updateRunning {x=0, y=0} game
    Lost -> case input of
      SpaceInput True -> { makeGame | state <- Running }
      _               -> game
    Won -> case input of
      SpaceInput True -> { makeGame | state <- Running }
      _               -> game


genericScreen : String -> String -> Element
genericScreen borderColor heading =
  toElement 400 300 <|
  div [ style [("background", "#eee")
              , ("box-shadow", "5px 5px 0px 0px #888")
              , ("padding", "10px")
              , ("border", "2px solid")
              , ("border-color", borderColor)
              ]]
  [ h2 [style [("margin", "0")]] [text heading]
  , p [] [text "Land gently on the red platform before running out of fuel."]
  , p [] [text "Use < and > to roll the ship, ^ to boost."]
  , p [] [text "Press SPACE to start."]
  ]

startScreen = genericScreen "#eee" "Rocket lander in Elm."
lostScreen = genericScreen "#a00" "Ouch!"
wonScreen = genericScreen "#0a0" "Good job, commander."

statsScreen : (Int, Int) -> Game -> Element
statsScreen (w, h) game =
  let
    speedColor = ("color", if shipSpeed game.ship < speedCutoff then "#0a0" else "#f00")
    rollColor = ("color", if abs(game.ship.roll) < rollCutoff then "#0a0" else "#f00")
  in
  container w h topRight <| toElement 200 100 <|
  div [style [("font-family", "monospace"), ("color", "#fff")]]
  [ p [] [ text ("Fuel: " ++ toString game.ship.fuel) ]
  , p [style [speedColor]] [ text ("Speed: " ++ String.slice 0 6 (toString (shipSpeed game.ship))) ]
  , p [style [rollColor]] [ text ("Roll: " ++ String.slice 0 6 (toString (game.ship.roll))) ]
  ]

updateRunning : {x: Int, y: Int} -> Game -> Game
updateRunning arrows game =
  let
    ship = shipUpdate
           game.gravity
           (if game.ship.fuel > 0 then arrows.y == 1 else False)
           (if game.ship.fuel > 0 then arrows.x else 0)
           game.ship
    (platformPos, landscape) = generateLandscape
  in
    case (isShipAlive (ship.x, ship.y) landscape, isShipLanded ship platformPos) of
      (True, False)  -> {game | ship <- ship}
      (True, True)   -> {game | state <- Won}
      (False, True)  -> {game | state <- Lost}
      (False, False) -> {game | state <- Lost}


shipUpdate : Float -> Bool -> Int -> Ship -> Ship
shipUpdate g boosting roll ship =
  let
    accell = { x = if boosting then -3.0 * g * sin(ship.roll) else 0
             , y = if boosting then 3.0 * g * cos(ship.roll) else 0
             }
    fuel' = if boosting then ship.fuel - 3 else ship.fuel
    fuel'' = if abs roll > 0 then fuel' - 1 else fuel'
  in
    { ship | vy <- ship.vy - g + accell.y
           , vx <- ship.vx + accell.x
           , y <- ship.y + ship.vy
           , x <- ship.x + ship.vx
           , boosting <- boosting
           , fuel <- fuel''
           , roll <- ship.roll - ( (toFloat roll) / 20.0)
           }

paint : Game -> (Int, Int) -> Element
paint game (w, h) =
  case game.state of
    PreRunning -> collage w h
      [ toForm (paintGame game (w, h))
      , toForm startScreen
      ]
    Running -> paintGame game (w, h)
    Lost -> collage w h
      [ toForm (paintGame game (w, h))
      , toForm lostScreen
      ]
    Won -> collage w h
      [ toForm (paintGame game (w, h))
      , toForm wonScreen
      ]

shipSpeed : Ship -> Float
shipSpeed ship = clamp 0.0 1.0 (sqrt (ship.vx * ship.vx + ship.vy * ship.vy))

isShipLanded : Ship -> Float -> Bool
isShipLanded ship platformPos =
  abs ship.y < 0.01
    && abs (ship.x - platformPos) < 0.1
    && shipSpeed ship < speedCutoff
    && abs ship.roll < rollCutoff

{- check whether the ship is below a list of half-planes -}
isShipAlive : (Float, Float) -> List (Float, Float) -> Bool
isShipAlive (sx, sy) landscape =
  let hits = List.foldl (isHit (sx, sy)) (False, (0, 0)) landscape
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
    (hitSoFar || (nx * sx + ny * sy < 0 && shipX >= prevX && shipX < newX), (newX, newY))

toScreenCoords : (Int, Int) -> (Float, Float) -> (Float, Float)
toScreenCoords (w, h) (x, y)=
  ((x - 0.5) * (toFloat w), (y - 0.5) * (toFloat h))

paintGame : Game -> (Int, Int) -> Element
paintGame game (w, h) =
  let fw = toFloat w
      fh = toFloat h
  in
  collage w h
  [ filled Color.black(rect fw fh)
  , paintLandscape generateLandscape (w, h)
  , paintPlatform game.platformPos (w, h)
  , paintShip game.ship (w, h)
  , toForm (statsScreen (w, h) game)
  ]

paintShip : Ship -> (Int, Int) -> Form
paintShip ship (w, h) =
  let
    color = Color.rgba 255 0 0 255
  in
  rotate (ship.roll) <|
  move (0, 20) <| {- Paint a bit higher so it looks like the ship dies on contact, not when deep in the land -}
  move (toScreenCoords (w, h) (ship.x, ship.y)) <|
  filled color (polygon [(0, 0), (10, -10), (10, -20), (-10, -20), (-10, -10)])

paintLandscape : (Float, List (Float, Float)) -> (Int, Int) -> Form
paintLandscape (platformPos, heights) (w, h) =
  let closedLoop = heights ++ [(1.0, 0.0), (0.0, 0.0)]
  in
  filled (Color.rgba 0 255 0 255)
    (polygon (List.map  (toScreenCoords (w, h)) closedLoop))

paintPlatform : Float -> (Int, Int) -> Form
paintPlatform platformPos (w, h) =
  let pos = (toFloat w) * (platformPos - 0.5)
  in
    move (pos, -(toFloat h / 2) + 5) <|
    filled (Color.rgba 255 0 0 255) (rect 80 10)

{- Make a landscape with a platform position and some mountain-looking
   things -}
generateLandscape : (Float, List (Float, Float))
generateLandscape =
  let
    platformIndex = Random.int 0 2
    mainValues = [Random.float 0.1 0.2, Random.float 0.4 0.5, Random.float 0.7 0.9]
  in
   (0.5, [(0.0, 0.2), (0.2, 0.4), (0.3, 0.6), (0.4, 0.1), (0.48, 0.0), (0.5, 0.0), (0.52, 0.0), (0.6, 0.2), (0.8, 0.6), (1.0, 0.7)])
