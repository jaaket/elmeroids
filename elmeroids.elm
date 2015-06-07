import Window
import Mouse
import Keyboard
import Debug
import Color
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time exposing (..)
import Signal exposing (..)

type alias Vec2 = (Float, Float)
type alias Object a = { a | pos:Vec2, velocity:Vec2, acceleration:Vec2,
                        angle:Float, angularVelocity: Float }
type alias Input = { up:Bool, down:Bool, left:Bool, right:Bool }

type alias Ship = Object {}

speed = 0.001
angularSpeed = 0.005

initShip : Ship
initShip = { pos=(0,0), velocity=(0,0), acceleration=(0,0),
             angle=0, angularVelocity=1 }

triangle : Form
triangle = outlined (solid Color.black)
                    (polygon [(20, 0), (-10, -10), (-10, 10)])


sum : Vec2 -> Vec2 -> Vec2
sum (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

integrate : Float -> Vec2 -> Vec2
integrate dt x = (fst x * dt, snd x * dt)

maneuver : Input -> Ship -> Ship
maneuver input ship =
  { ship | angularVelocity <- if | input.left  -> angularSpeed
                                 | input.right -> -angularSpeed
                                 | otherwise   -> 0,
           acceleration    <- if | input.up -> (speed * (cos ship.angle),
                                                speed * (sin ship.angle))
                                 | otherwise   -> (0, 0)
  }

simulate : Float -> Object a -> Object a
simulate dt obj =
  { obj | pos      <- sum obj.pos (integrate dt obj.velocity),
          velocity <- sum obj.velocity (integrate dt obj.acceleration),
          angle    <- obj.angle + dt * obj.angularVelocity
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

updateShip : ((Int, Int), Time, Input) -> Ship -> Ship
updateShip (windowSize, dt, input) = maneuver input >> simulate dt
                                                    >> wrapAround windowSize

updateGame : Signal Ship
updateGame = foldp updateShip
                   initShip
                   (map3 (,,) Window.dimensions (fps 60) keyInput)

keyInput : Signal Input
keyInput = map (\keys -> { up = keys.y == 1,
                           down = keys.y == -1,
                           left = keys.x == -1,
                           right = keys.x == 1 })
               Keyboard.arrows

main : Signal Element
main =
  map2 (\windowSize ship ->
    (uncurry collage windowSize [ move ship.pos (rotate ship.angle triangle) ]))
    Window.dimensions updateGame

