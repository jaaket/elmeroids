module Utils where

iterate : Int -> (a -> a) -> a -> a
iterate n f x = if | n > 0     -> f (iterate (n - 1) f x)
                   | otherwise -> x

