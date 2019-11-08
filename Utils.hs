
module Utils where

combine :: ([a]->a) -> [a] -> a
combine _ [x] = x
combine f xs = f xs
