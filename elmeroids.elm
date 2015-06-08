import Window
import Mouse
import Keyboard
import Debug
import Color
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time exposing (..)
import Signal exposing (..)
import Random exposing (..)
import Text exposing (fromString)

import Types exposing (Vec2, sum)
import Physics exposing (Object, simulate, collides)
import Utils exposing (iterate)

type alias Input = { up:Bool, down:Bool, left:Bool, right:Bool }

type GameState = Active | Over

type alias Ship = Object {}
type alias Asteroid = Object {}
type alias Game = { ship:Ship,
                    asteroids:List Asteroid,
                    windowSize:(Int, Int),
                    seed:Seed,
                    state:GameState }

speed = 0.001
angularSpeed = 0.005

initShip : Ship
initShip = { pos=(0,0), velocity=(0,0), acceleration=(0,0),
             angle=0, angularVelocity=0, collisionRadius=10 }

initGame : Game
initGame =
  iterate 10 addRandomAsteroid { ship=initShip,
                                 asteroids=[],
                                 windowSize=(1000, 1000),
                                 seed=initialSeed 0,
                                 state=Active }

addRandomAsteroid : Game -> Game
addRandomAsteroid game =
  let xmin = -(fst game.windowSize |> toFloat) / 2
      xmax =  (fst game.windowSize |> toFloat) / 2
      ymin = -(snd game.windowSize |> toFloat) / 2
      ymax =  (snd game.windowSize |> toFloat) / 2
      (pos, seed) = generate (pair (float xmin xmax) (float ymin ymax)) game.seed
      (vel, seed') = generate (pair (float -0.2 0.2) (float -0.2 0.2)) seed
      (angle, seed'') = generate (float 0 360) seed'
      (angularVel, seed''') = generate (float -0.01 0.01) seed''
  in  { game | asteroids <- { pos=pos, velocity=vel, acceleration=(0,0),
                              angle=angle, angularVelocity=angularVel,
                              collisionRadius=20 } :: game.asteroids,
               seed <- seed''' }

triangle : Form
triangle = outlined (solid Color.black)
                    (polygon [(20, 0), (-10, -10), (-10, 10)])

asteroid : Form
asteroid = outlined (solid Color.black) (ngon 5 20)

maneuver : Input -> Ship -> Ship
maneuver input ship =
  { ship | angularVelocity <- if | input.left  -> angularSpeed
                                 | input.right -> -angularSpeed
                                 | otherwise   -> 0,
           acceleration    <- if | input.up -> (speed * (cos ship.angle),
                                                speed * (sin ship.angle))
                                 | otherwise   -> (0, 0)
  }

wrapAround : (Int, Int) -> Object a -> Object a
wrapAround (w, h) obj =
  let x = fst obj.pos
      y = snd obj.pos
  in  { obj | pos <- ((if | x < -(toFloat w) / 2 -> x + toFloat w
                          | x > toFloat w / 2    -> x - toFloat w
                          | otherwise            -> x,
                       if | y < -(toFloat h) / 2 -> y + toFloat h
                          | y > toFloat h / 2    -> y - toFloat h
                          | otherwise            -> y))
      }

updateShip : (Int, Int) -> Time -> Input -> Ship -> Ship
updateShip windowSize dt input = maneuver input >> simulate dt
                                                >> wrapAround windowSize

updateAsteroid : (Int, Int) -> Time -> Asteroid -> Asteroid
updateAsteroid windowSize dt = simulate dt >> wrapAround windowSize

updateGame : ((Int, Int), Time, Input) -> Game -> Game
updateGame (windowSize, dt, input) game =
  case game.state of
    Active -> { game | ship <- updateShip windowSize dt input game.ship,
                       asteroids <- List.map (updateAsteroid windowSize dt) game.asteroids,
                       state <- if List.any (collides game.ship) game.asteroids
                                  then Over
                                  else Active
      }
    Over -> game

gameState : Signal Game
gameState = foldp updateGame
                  initGame
                  (map3 (,,) Window.dimensions (fps 60) keyInput)

keyInput : Signal Input
keyInput = map (\keys -> { up = keys.y == 1,
                           down = keys.y == -1,
                           left = keys.x == -1,
                           right = keys.x == 1 })
               Keyboard.arrows

renderObject : Form -> Object a -> Form
renderObject form obj = move obj.pos (rotate obj.angle form)

render : (Int, Int) -> Game -> Element
render (w, h) game =
  let ship = renderObject triangle game.ship
      asteroids = List.map (renderObject asteroid) game.asteroids
      world = collage w h <| ship :: asteroids
      overlay = fromString "Game over!"
                  |> Text.typeface ["lato", "sans-serif"]
                  |> Text.color Color.white
                  |> Text.height 40
                  |> centered
                  |> container w h middle
                  |> color Color.black
                  |> opacity 0.9
  in  case game.state of
           Active -> world
           Over   -> layers [ world, overlay ]

main : Signal Element
main = map2 render Window.dimensions gameState

