{-# LANGUAGE GADTs #-}

import Data.Char
import Data.String
import Data.List as List
import Data.Map as Map
import Text.Parsec as P
import Text.Parsec.Pos as Pos
import Text.Parsec.String (Parser)
import Text.Parsec.Language (haskellDef)
import Text.Parsec.Expr

import qualified Text.Parsec.Token as Tok

import Debug.Trace


xs = [1,2,3,4,5]
ys = "ab"

zips = [(x, y) |x<-xs, y<-ys]

zs = do
  x <- xs
  y <- ys
  [(x,y)]
