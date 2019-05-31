{-# LANGUAGE GADTs #-}

import Data.Char
import Data.String
import Data.List as List
import Data.Map as Map
import Text.Parsec as P
import Text.Parsec.Pos as Pos
import Text.Parsec.String (Parser)
import Text.Parsec.Language (haskellDef)

import qualified Text.Parsec.Token as Tok

import Debug.Trace

langDef :: Tok.LanguageDef ()
langDef = Tok.LanguageDef
  { Tok.commentStart    = "{-"
  , Tok.commentEnd      = "-}"
  , Tok.commentLine     = "--"
  , Tok.nestedComments  = True
  , Tok.identStart      = letter
  , Tok.identLetter     = alphaNum <|> oneOf "_'"
  , Tok.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Tok.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Tok.reservedNames   = ["case","of","if","then","else","data","type","class","import","module"]
  , Tok.reservedOpNames = ["::","=","<-","->","@"]
  , Tok.caseSensitive   = True
  }


lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser haskellDef

p1 = do
  char 'a'
  char 'b'
  return 0

p2 = do {
  char 'c'
    ;char 'd'
 ;return 1
  }


p0 :: Parser Int
p0 = p1 <|> p2

parseSExp :: String -> Either ParseError Int
parseSExp s = parse p0 "<stdin>" s


data PTerm 
  = Operator String
  | Number Integer
  | Expr [PTerm]
  deriving (Show)

isOperator (Operator _) = True
isOperator _ = False
  

priority = Map.fromList 
 [  ("=",1),
    ("<-",1),
    ("->",1),
    ("::",2),
    ("+",6),
    ("-",6),
    ("*",7),
    ("/",7)
 ]

expr = [Number 1, Operator "+", Number 2, Number 2, Operator "*", Number 3, Number 5, Operator "-", Number 4]

foldExpr' :: [PTerm] -> [PTerm]
foldExpr' [x] = [x]
foldExpr' [a, op@(Operator _), b] = [Expr [a, op, b]]
foldExpr' (a : op0@(Operator op0Name) : xss@(b : op1@(Operator op1Name) : xs)) = 
  if (priority ! op0Name) >= (priority ! op1Name) 
  then Expr [a,op0,b] : op1 : xs
  else foldExpr' (a : op0 : foldExpr' xss)

foldExpr' (a : op@(Operator _) : xs) =
  foldExpr' (a : op : foldExpr' xs)

foldExpr' xs = Expr call:last'
  where (call, last') = List.break isOperator xs

foldExpr :: PTerm -> PTerm
foldExpr (Expr xs) = head $ head $ List.dropWhile (not . List.null . tail) $ iterate foldExpr' xs
 

main = do {return ();}

