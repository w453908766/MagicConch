module Utils where

import Data.List as List
import Text.Parsec.Prim
import Text.Parsec.Pos

while :: (a -> Bool) -> (a -> a) -> (a -> a)
while pred f x 
 |pred x = while pred f (f x)
 |otherwise = x


isSingle [_] = True
isSingle _ = False

satisfy' :: (Show a) => (a -> Bool) -> Parsec [a] s a
satisfy' f = tokenPrim show (\pos _ _ -> incSourceColumn pos 1)
                      (\x -> if f x then Just x else Nothing)

