module SExp where

import Utils

data SExp a
  = Symbol a String
  | Lit a Integer
  | Refer a (SExp a)
  | SList [SExp a]
  deriving (Eq, Ord, Show, Read)

instance Monoid (SExp a) where
  mempty = SList []
  mappend (SList xs) (SList ys) = SList (xs++ys)

getSList (SList xs) = xs

showSExp' :: SExp Int -> ShowS
showSExp' (Symbol _ name) = mappend name
showSExp' (Lit _ num) = shows num
showSExp' (Refer _ quote) = (mappend "(Refer ") . (showSExp' quote) . (showChar ')')
showSExp' (SList []) = mappend "()"
showSExp' (SList (x:xs)) = 
  \s -> showChar '(' $ showSExp' x $ foldr showSpace  (')':s) xs
  where showSpace t = (showChar ' ') . (showSExp' t)
    
showSExp = flip showSExp' ""


satSymbol f (Symbol _ x) = f x
satSymbol _ _ = False
     
isSymbol s = satSymbol (==s)


