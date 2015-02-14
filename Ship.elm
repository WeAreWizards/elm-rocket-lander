import Color(..)
import Graphics.Element (..)
import Graphics.Collage (..)
import Signal
import Window
import Keyboard
import Html (..)
import Html.Attributes (..)
import Html.Events (..)
import Html.Lazy (lazy, lazy2)
import String
import Time(fps)

type alias Radians = Float

type alias Ship =
  { x: Float
  , y: Float
  , vx: Float
  , vy: Float
  , roll: Radians
  , boosting: Bool
  }

type alias Game =
  { gravity: Float
  , ship: Ship
  }


makeGame : Game
makeGame = Game 0.098 (Ship 0 0 0 0 0 False)


main : Signal Element
main =
  Signal.map2 paint model Window.dimensions


model : Signal Game
model = Signal.foldp update makeGame (Signal.sampleOn (fps 30) Keyboard.arrows)


update : {x: Int, y: Int} -> Game -> Game
update arrows game =
  let
    ship = shipUpdate game.gravity (arrows.y == 1) (arrows.x)  game.ship
  in
    {game | ship <- ship }

shipUpdate : Float -> Bool -> Int -> Ship -> Ship
shipUpdate g boosting roll ship =
  let
    accell = { x = if boosting then -3.0 * g * sin(ship.roll) else 0
             , y = if boosting then 3.0 * g * cos(ship.roll) else 0
             }
  in
    { ship | vy <- ship.vy - g + accell.y
           , vx <- ship.vx + accell.x
           , y <- ship.y + ship.vy
           , x <- ship.x + ship.vx
           , boosting <- boosting
           , roll <- ship.roll - ( (toFloat roll) / 20.0)
           }


paint : Game -> (Int, Int) -> Element
paint game (w, h) =
  collage w h
  [ paintShip game.ship
  ]

paintShip : Ship -> Form
paintShip ship =
  rotate (ship.roll) <|
  move (ship.x, ship.y) <|
  filled (rgba 255 0 0 255) (polygon [(0, 0), (10, -10), (10, -20), (-10, -20), (-10, -10)])
