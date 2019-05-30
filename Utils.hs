module Utils where

while :: (a->Bool) -> (a->a) -> (a->a)
while p t x 
 |p x = while p t (t x)
 |otherwise = x
