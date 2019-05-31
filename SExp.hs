module SExp where

import Utils

data SExp
  = Symbol String
  | Operator String
  | Number Integer
  | SList [SExp]
  deriving (Eq, Ord, Show, Read)

instance Monoid SExp where
  mempty = SList []
  mappend (SList xs) (SList ys) = SList (xs++ys)

getSList (SList xs) = xs

showSExp' :: SExp -> ShowS
showSExp' (Symbol name) = mappend name
showSExp' (Operator op) = mappend op
showSExp' (Number num) = shows num
showSExp' (SList []) = mappend "()"
showSExp' (SList (x:xs)) = 
  \s -> showChar '(' $ showSExp' x $ foldr showSpace  (')':s) xs
  where showSpace t = (showChar ' ') . (showSExp' t)
    
showSExp = flip showSExp' ""



mapChart :: ([SExp] -> [SExp]) -> (SExp -> SExp)
mapChart f (SList xs) = SList $ f $ map (mapChart f) xs
mapChart _ x = x

     
