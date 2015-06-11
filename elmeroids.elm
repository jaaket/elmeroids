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

type alias Input = { up:Bool, down:Bool, left:Bool, right:Bool, fire:Bool }

type alias Rect = { left:Float, right:Float, bottom:Float, top:Float }

windowBounds : (Int, Int) -> Rect
windowBounds (w, h) =
  { left   = -(toFloat w) / 2,
    right  =  (toFloat w) / 2,
    bottom = -(toFloat h) / 2,
    top    =  (toFloat h) / 2
  }

type GameState = Active | Over

type alias Ship = Object {}
type alias Asteroid = Object {}
type alias Bullet = Object {}
type alias Game = { ship:Ship,
                    asteroids:List Asteroid,
                    bullets:List Bullet,
                    bounds:Rect,
                    seed:Seed,
                    state:GameState,
                    shootCooldown:Float }

speed = 0.001
angularSpeed = 0.005
shootInterval = 500

initShip : Ship
initShip = { pos=(0,0), velocity=(0,0), acceleration=(0,0),
             angle=0, angularVelocity=0, collisionRadius=10 }

asteroidGenerator : Rect -> Generator Asteroid
asteroidGenerator bounds =
  customGenerator <| \seed ->
    let (pos, seed1) = generate (pair (float bounds.left bounds.right)
                                      (float bounds.bottom bounds.top)) seed
        (vel, seed2) = generate (pair (float -0.2 0.2) (float -0.2 0.2)) seed1
        (angle, seed3) = generate (float 0 360) seed2
        (angularVel, seed4) = generate (float -0.01 0.01) seed3
    in  ({ pos=pos, velocity=vel, acceleration=(0,0),
           angle=angle, angularVelocity=angularVel,
           collisionRadius=20 }, seed4)

initGame : Game
initGame =
  let seed = initialSeed 0
      bounds = { left = -500, right = 500, bottom = -500, top = 500 }
  in  { ship=initShip,
        asteroids=generate (list 10 (asteroidGenerator bounds)) seed |> fst,
        bullets=[],
        bounds=bounds,
        seed=seed,
        state=Active,
        shootCooldown = shootInterval }

maneuver : Input -> Ship -> Ship
maneuver input ship =
  { ship | angularVelocity <- if | input.left  -> angularSpeed
                                 | input.right -> -angularSpeed
                                 | otherwise   -> 0,
           acceleration    <- if | input.up -> (speed * (cos ship.angle),
                                                speed * (sin ship.angle))
                                 | otherwise   -> (0, 0)
  }

shoot : Ship -> Bullet
shoot { pos, angle } = { pos=pos,
                         velocity=(cos angle, sin angle),
                         acceleration=(0,0),
                         angle=angle,
                         angularVelocity=0,
                         collisionRadius = 5 }

wrapAround : Rect -> Object a -> Object a
wrapAround { left, right, bottom, top } obj =
  let x = fst obj.pos
      y = snd obj.pos
      w = right - left
      h = top - bottom
  in  { obj | pos <- ((if | x < left  -> x + w
                          | x > right -> x - w
                          | otherwise -> x,
                       if | y < bottom -> y + h
                          | y > top    -> y - h
                          | otherwise  -> y))
      }

updateShip : Rect -> Time -> Input -> Ship -> Ship
updateShip bounds dt input = maneuver input >> simulate dt
                                            >> wrapAround bounds

updateAsteroid : Rect -> Time -> Asteroid -> Asteroid
updateAsteroid bounds dt = simulate dt >> wrapAround bounds

updateBullet : Rect -> Time -> Bullet -> Bullet
updateBullet bounds dt = simulate dt >> wrapAround bounds

pieceGenerator : Asteroid -> Generator Asteroid
pieceGenerator ast =
  customGenerator <| \seed ->
    let (velChange, seed') = generate (pair (float -0.1 0.1) (float -0.1 0.1))
                                      seed
        piece = { ast | velocity <- sum ast.velocity velChange,
                        collisionRadius <- ast.collisionRadius * 0.7 }
    in  (piece, seed')

asteroidPieces : Asteroid -> List Asteroid
asteroidPieces ast =
  generate (list 3 (pieceGenerator ast))
           (initialSeed (floor <| (fst ast.pos * 1000 + snd ast.pos) * ast.angle))
    |> fst

splitAsteroidIfLargerThan : Float -> Asteroid -> List Asteroid
splitAsteroidIfLargerThan r ast =
  if ast.collisionRadius > r
     then asteroidPieces ast
     else []

updateWorld : (Rect, Time, Input) -> Game -> Game
updateWorld (bounds, dt, input) game =
  let newBullets = if input.fire && game.shootCooldown < 0
                      then [shoot game.ship]
                      else []
      (asteroidsDestroyed, asteroidsLeft) =
        List.partition (\asteroid -> List.any (collides asteroid) game.bullets)
                       game.asteroids
      bulletsLeft = List.filter (\bullet -> not <| List.any (collides bullet) game.asteroids)
                                game.bullets
  in  { game | ship          <- updateShip bounds dt input game.ship,
               asteroids     <- (List.map (updateAsteroid bounds dt)
                                          asteroidsLeft) ++
                                (List.concatMap (splitAsteroidIfLargerThan 10)
                                                asteroidsDestroyed),
               bullets       <- List.map (updateBullet bounds dt)
                                         (bulletsLeft ++ newBullets),
               shootCooldown <- if input.fire && game.shootCooldown < 0
                                   then shootInterval
                                   else game.shootCooldown - dt,
               state <- if List.any (collides game.ship) game.asteroids
                           then Over
                           else Active
      }

updateGame : ((Int, Int), Time, Input) -> Game -> Game
updateGame (windowSize, dt, input) game =
  case game.state of
    Active -> updateWorld (windowBounds windowSize, dt, input) game
    Over   -> game

gameState : Signal Game
gameState = foldp updateGame
                  initGame
                  (map3 (,,) Window.dimensions (fps 60) keyInput)

keyInput : Signal Input
keyInput = map2 (\arrows space -> { up    = arrows.y == 1,
                                    down  = arrows.y == -1,
                                    left  = arrows.x == -1,
                                    right = arrows.x == 1,
                                    fire  = space })
                Keyboard.arrows Keyboard.space

triangle : Form
triangle = outlined (solid Color.black)
                    (polygon [(20, 0), (-10, -10), (-10, 10)])

asteroid : Asteroid -> Form
asteroid ast = outlined (solid Color.black) (ngon 5 ast.collisionRadius)

bullet : Form
bullet = outlined (solid Color.black) (ngon 7 2)

renderObject : Form -> Object a -> Form
renderObject form obj = move obj.pos (rotate obj.angle form)

renderAsteroid : Asteroid -> Form
renderAsteroid ast = renderObject (asteroid ast) ast

render : (Int, Int) -> Game -> Element
render (w, h) game =
  let ship = renderObject triangle game.ship
      asteroids = List.map renderAsteroid game.asteroids
      bullets = List.map (renderObject bullet) game.bullets
      world = collage w h <| ship :: asteroids ++ bullets
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

