module Models exposing (..)

import Random


type alias Radians = Float

type GameState =  PreRunning 
                | Running 
                | Won 
                | Lost

type alias Model =
  { gravity: Float
  , ship: Ship
  , state: GameState
  , platformPos: Float
  , height: Int
  , width: Int
  }

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


