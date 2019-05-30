module SExp where

import Control.Monad.Identity
import Control.Applicative
import Control.Monad.State
import Control.Monad
import Data.Char
import qualified Data.List as List

import Debug.Trace


data SExp a = Atom {getAtom :: a} | SList {getSList :: [SExp a]} deriving (Eq, Ord)

instance (Show a) => Show (SExp a) where
  showsPrec _ (Atom x) = shows x
  showsPrec _ (SList xs) = shows xs

instance (Read a) => Read (SExp a) where
  readsPrec _ xs@('[':_) = [(SList s, xs')]
    where [(s, xs')] = reads xs
  readsPrec _ xs =  [(Atom s, xs')]
    where [(s, xs')] = reads xs

instance Monoid (SExp a) where
  mempty = nil
  mappend (SList xs) (SList ys) = SList (xs++ys)

instance Foldable SExp where
  foldr f acc (Atom x) = f x acc
  foldr f acc (SList xs) = foldr (flip (foldr f)) acc xs

instance Traversable SExp where
  traverse f (Atom x) = fmap Atom (f x)
  traverse f (SList xs) = fmap SList $ traverse (traverse f) xs

instance Functor SExp where
  fmap = liftM
  
instance Applicative SExp where
  pure = return
  (<*>) = ap

instance Monad SExp where
  return = Atom
  (Atom x) >>= f = f x
  (SList xs) >>= f = SList (map (>>= f) xs) 


mapChart :: ([SExp a] -> [SExp a]) -> (SExp a -> SExp a)
mapChart f (Atom x) = Atom x
mapChart f (SList xs) = SList $ f $ map (mapChart f) xs


elemIndices x sexp = filter (/=[]) $ foldr (:) [] $ zipSExpWith (\y i->if x==y then i else []) sexp (getIndexs [] sexp)


zipSExpWith :: (a -> b -> c) -> (SExp a -> SExp b -> SExp c)
zipSExpWith f (Atom x) (Atom y) = Atom (f x y)
zipSExpWith f (SList xs) (SList ys) = SList $ zipWith (zipSExpWith f) xs ys


getIndexs :: [Int] -> SExp a -> SExp [Int]
getIndexs index (Atom _) = Atom (reverse index)
getIndexs index (SList xs) = SList $ zipWith (\sexp i->getIndexs (i:index) sexp) xs [0..]


cons :: SExp a -> SExp a -> SExp a
cons car (SList cdr) = SList (car:cdr)

car, cdr :: SExp a -> SExp a
car (SList (x:_)) = x
cdr (SList (_:xs)) = SList xs

nil = SList [] :: SExp a


listLens :: Functor f => Int -> (a -> f a) -> ([a] -> f [a])
listLens 0 f (x:xs) = fmap (:xs) (f x) 
listLens n f (x:xs) = fmap (x:) (listLens (n-1) f xs)

sexpLens :: (Functor f) => [Int] -> (a -> f a) -> ((SExp a) -> f (SExp a))
sexpLens [] f (Atom x) = fmap Atom (f x)
sexpLens (index:indexs) f (SList xs) = fmap SList $ listLens index (sexpLens indexs f) xs

setSExp f sexp indexs = runIdentity $ sexpLens indexs (Identity . f) sexp

toList :: SExp a -> [a]
toList (SList as) = map getAtom as

fromList :: [a] -> SExp a
fromList xs = SList (map Atom xs)


s = SList [Atom 1, Atom 2, SList [Atom 3, SList [Atom 4]], Atom 5]
a = SList [Atom 2, SList [Atom 3]]
b = SList [Atom 1, SList [Atom 4, Atom 5]]
fa = SList [Atom (*2), SList [Atom (+3)]]

z = fromList [1..3]
y = map (Atom . Just) [1,2]




