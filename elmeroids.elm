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
type alias Ship = { pos:Vec2, angle:Float, speed:Vec2, acceleration:Vec2 }
type alias Input = { up:Bool, down:Bool, left:Bool, right:Bool }

speed = 0.1
angularSpeed = 0.01

initShip : Ship
initShip = { pos=(0,0), angle=0, speed=(0,0), acceleration=(0,0) }

triangle : Form
triangle = outlined (solid Color.black)
                    (polygon [(20, 0), (-10, -10), (-10, 10)])


sum : Vec2 -> Vec2 -> Vec2
sum (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

integrate : Float -> Vec2 -> Vec2
integrate dt x = (fst x * dt, snd x * dt)

updateAcceleration : Input -> Float -> Vec2
updateAcceleration input angle =
  if | input.up  -> (0.001 * cos angle, 0.001 * sin angle)
     | otherwise -> (0, 0)

updateSpeed : Float -> Ship -> Vec2
updateSpeed dt ship =
  sum ship.speed (integrate dt ship.acceleration)

updateAngle : Input -> Float -> Float
updateAngle input angle =
  if | input.left  -> angle + 0.1
     | input.right -> angle - 0.1
     | otherwise   -> angle

updateShip : (Time, Input) -> Ship -> Ship
updateShip (dt, input) ship =
  { ship | pos <- sum ship.pos (integrate dt ship.speed),
           speed <- updateSpeed dt ship,
           acceleration <- updateAcceleration input ship.angle,
           angle <- updateAngle input ship.angle
  }

keyInput : Signal Input
keyInput = map (\keys -> { up = keys.y == 1,
                           down = keys.y == -1,
                           left = keys.x == -1,
                           right = keys.x == 1 })
               Keyboard.arrows

updateGame : Signal Ship
updateGame = foldp updateShip
                   initShip
                   (map2 (,) (fps 60) keyInput)

main : Signal Element
main =
  map (\ship -> (collage 600 600 [ move ship.pos (rotate ship.angle triangle) ])) updateGame

