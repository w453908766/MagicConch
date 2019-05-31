module Parser where

import Data.Char
import Data.String
import qualified Data.List as List
import qualified Data.Map as Map
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
 [  ("=", (2, False)),
    ("<-", (2, False)),
    ("::", (2, False)),

    ("->", (2, False)),

    ("+", (6, True)),
    ("-", (6, True)),
    ("*", (7, True)),
    ("/", (7, True))
 ]

isOperator :: SExp -> Bool
isOperator (Operator x) = True
isOperator _ = False

foldCall [x] = x
foldCall xs = SList xs

zipOperator [] = []
zipOperator [x] = [(x, Operator "")]
zipOperator (x:op:xs) = (x, op):(zipOperator xs)


mapPrior [_] _ _ = [(0, 0)]
mapPrior ((_, Operator op):xs) left right
 |leftComb  = (prior, left):(mapPrior xs (left-1) right)
 |otherwise = (prior, right):(mapPrior xs left (right+1))
  where (prior, leftComb) = (Map.!) priority op
  

foldOperator (a, op0) (b, op1) = (SList [a,op0,b], op1)

foldSExp :: SExp -> SExp
foldSExp (SList []) = SList []
foldSExp (SList xs) = ret
  where
    xs' = map foldSExp xs 
    groups = List.groupBy (on (==) isOperator) xs' :: [[SExp]]
    xs'' = map foldCall groups :: [SExp]
    zip' = zipOperator xs'' :: [(SExp, SExp)]
    priors = mapPrior zip' 0 0
    (ret, _) = foldExpr foldOperator priors zip'
foldSExp x = x


parseSExp :: String -> SExp
parseSExp s = 
  case parse pSExp "<stdin>" s of
  Left e  -> Symbol (show e)
  Right x -> foldSExp x


code = "(f :: Int -> Int -> Int) (f a b c = {d=a+b*c;return d()})"
scode = "((f a b c) = ((d = (a+(b*c))) (return (d()))))"
sexp = SList [SList [Symbol "f",Symbol "a",Symbol "b",Symbol "c"],Operator "=",SList [SList [Symbol "d",Operator "=",SList [Symbol "a",Operator "+",SList [Symbol "b",Operator "*",Symbol "c"]]],SList [Symbol "return",SList [Symbol "d",SList []]]]]
