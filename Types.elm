module Types where

type alias Vec2 = (Float, Float)

sum : Vec2 -> Vec2 -> Vec2
sum (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

dist : Vec2 -> Vec2 -> Float
dist (x1, y1) (x2, y2) = sqrt ((x1 - x2)*(x1 - x2) + (y1 - y2)*(y1 - y2))

