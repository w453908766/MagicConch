module Utils where
import Data.List as List

while :: (a -> Bool) -> (a -> a) -> (a -> a)
while pred f x 
 |pred x = while pred f (f x)
 |otherwise = x



foldExpr' :: Ord a => (b -> b -> b) -> [a] -> [b] -> [a] -> [b] -> b

foldExpr' _ [_] [x] [] [] = x

foldExpr' f [] [] (op:ops) (y:ys) = 
  foldExpr' f [op] [y] ops ys

foldExpr' f (op0:ops0) (x:xs) (op1:ops1) (y:ys)
 |op0 >= op1 = foldExpr' f ops0 xs (op1:ops1) (f x y:ys)
 |otherwise  = foldExpr' f  (op1:op0:ops0) (y:x:xs) ops1 ys

foldExpr :: Ord a => (b -> b -> b) -> [a] -> [b] -> b
foldExpr f ops ys = foldExpr' f [] [] ops ys


isSingle [_] = True
isSingle _ = False
