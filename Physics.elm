module Physics where

import Types exposing (Vec2, sum, dist)

type alias Object a = { a | pos:Vec2, velocity:Vec2, acceleration:Vec2,
                            angle:Float, angularVelocity: Float,
                            collisionRadius:Float }

integrate : Float -> Vec2 -> Vec2
integrate dt x = (fst x * dt, snd x * dt)

simulate : Float -> Object a -> Object a
simulate dt obj =
  { obj | pos      <- sum obj.pos (integrate dt obj.velocity),
          velocity <- sum obj.velocity (integrate dt obj.acceleration),
          angle    <- obj.angle + dt * obj.angularVelocity
  }

collides : Object a -> Object b -> Bool
collides obj1 obj2 = dist obj1.pos obj2.pos < obj1.collisionRadius + obj2.collisionRadius

