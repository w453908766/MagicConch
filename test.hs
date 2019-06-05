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

data Expr 
  = Lit Char 
  | Add Expr Expr 
  | Mul Expr Expr
  deriving (Show)

table = 
  [ [Infix (char '*' >> return Mul) AssocNone]
  , [Infix (char '+' >> return Add) AssocNone]
  ]

expr :: Parser Expr
expr = buildExpressionParser table (fmap Lit digit)


dd :: Parsec [Int] () Char
dd = token show (const (newPos "dsfs" 0 0)) (const (Just 'c'))

token' :: (Show a) => Parsec [a] s a
token' = token show (const (newPos "" 0 0)) Just

--test :: Parsec [Int] () Int
--test = many 
