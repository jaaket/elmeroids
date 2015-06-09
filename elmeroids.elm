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

type GameState = Active | Over

type alias Ship = Object {}
type alias Asteroid = Object {}
type alias Bullet = Object {}
type alias Game = { ship:Ship,
                    asteroids:List Asteroid,
                    bullets:List Bullet,
                    windowSize:(Int, Int),
                    seed:Seed,
                    state:GameState,
                    shootCooldown:Float }

speed = 0.001
angularSpeed = 0.005
shootInterval = 500

initShip : Ship
initShip = { pos=(0,0), velocity=(0,0), acceleration=(0,0),
             angle=0, angularVelocity=0, collisionRadius=10 }

initGame : Game
initGame =
  iterate 10 addRandomAsteroid { ship=initShip,
                                 asteroids=[],
                                 bullets=[],
                                 windowSize=(1000, 1000),
                                 seed=initialSeed 0,
                                 state=Active,
                                 shootCooldown = shootInterval }

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

updateBullet : (Int, Int) -> Time -> Bullet -> Bullet
updateBullet windowSize dt = simulate dt >> wrapAround windowSize

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

updateWorld : ((Int, Int), Time, Input) -> Game -> Game
updateWorld (windowSize, dt, input) game =
  let newBullets = if input.fire && game.shootCooldown < 0
                      then [shoot game.ship]
                      else []
      (asteroidsDestroyed, asteroidsLeft) =
        List.partition (\asteroid -> List.any (collides asteroid) game.bullets)
                       game.asteroids
      bulletsLeft = List.filter (\bullet -> not <| List.any (collides bullet) game.asteroids)
                                game.bullets
  in  { game | ship          <- updateShip windowSize dt input game.ship,
               asteroids     <- (List.map (updateAsteroid windowSize dt)
                                          asteroidsLeft) ++
                                (List.concatMap (splitAsteroidIfLargerThan 10)
                                                asteroidsDestroyed),
               bullets       <- List.map (updateBullet windowSize dt)
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
    Active -> updateWorld (windowSize, dt, input) game
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

