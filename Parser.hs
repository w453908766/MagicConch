module Parser where

import Data.Char
import Data.String
import Data.List as List
import Data.Map as Map
import Data.Function

import Text.Parsec as P
import Text.Parsec.Pos
import Text.Parsec.String (Parser)
import Text.Parsec.Language (haskellDef)
import qualified Text.Parsec.Token as Tok

import Debug.Trace

import Utils
import SExp

opChars = ":!#$%&*+./<=>?@\\^|-~"

langDef :: Tok.LanguageDef ()
langDef = Tok.LanguageDef
  { Tok.commentStart    = "{-"
  , Tok.commentEnd      = "-}"
  , Tok.commentLine     = "--"
  , Tok.nestedComments  = True
  , Tok.identStart      = letter
  , Tok.identLetter     = alphaNum <|> oneOf "_'"
  , Tok.opStart         = oneOf opChars
  , Tok.opLetter        = oneOf opChars
  , Tok.reservedNames   = ["case","of","if","then","else","data","type","class","import","module","return"]
--  , Tok.reservedOpNames = ["::","=","<-","@", ";", "+", "-", "*", "/", "%"]
  , Tok.reservedOpNames = [";"]
  , Tok.caseSensitive   = True
  }


lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef

pOperator = Operator <$> Tok.operator lexer
pSymbol = Symbol <$> Tok.identifier lexer
pNumber = Number <$> Tok.integer lexer
pAtom = pOperator <|> pSymbol <|> pNumber

pKey key = do
  spaces
  Tok.reserved lexer key
  ret <- pSExp
  return (SList [Symbol key, ret])

pBlock = SList <$> Tok.braces lexer (Tok.semiSep lexer pSExp)

pIf = do
  cond' <- pKey "if"
  then' <- pKey "then"
  else' <- option (SList []) (pKey "else")
  return $ mappend cond' $ mappend then' else'
  
 
pItem = (Tok.parens lexer pSExp) <|> pBlock <|> pIf <|> pKey "return" <|> pAtom

pSExp = spaces >> SList <$> sepBy pItem spaces


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

isOperator :: SExp -> Bool
isOperator (Operator x) = True
isOperator _ = False

foldExpr' :: [SExp] -> [SExp]
foldExpr' [x] = [x]

foldExpr' [a, op@(Operator _), b] = [SList [a, op, b]]

foldExpr' (a : op0@(Operator op0Name) : xss@(b : op1@(Operator op1Name) : xs)) = 
  if (priority ! op0Name) >= (priority ! op1Name) 
  then SList [a,op0,b] : op1 : xs
  else foldExpr' (a : op0 : foldExpr' xss)

foldExpr' (a : op@(Operator _) : xs) =
  foldExpr' (a : op : foldExpr' xs)

foldExpr' xs = SList call:last'
  where (call, last') = List.break isOperator xs

foldExpr :: [SExp] -> [SExp]
foldExpr [SList xs] = xs
foldExpr xs = foldExpr (foldExpr' xs)


parseSExp :: String -> Either ParseError SExp
parseSExp s = fmap (mapChart foldExpr) (parse pSExp "<stdin>" s)



code = "f a b c = {d=a+b*c;return d()}"
scode = "((f a b c) = ((d = (a+(b*c))) (return d)))"
sexp = SList [SList [Symbol "f",Symbol "a",Symbol "b",Symbol "c"],Operator "=",SList [SList [Symbol "d",Operator "=",Symbol "a",Operator "+",Symbol "b",Operator "*",Symbol "c"],SList [Symbol "return",Symbol "d"]]]

