module Parser where

import Data.Char
import Data.String
import Data.Functor.Identity
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Function

import Text.Parsec as P
import Text.Parsec.Expr
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

pOperator = Symbol <$> Tok.operator lexer
pSymbol = Symbol <$> Tok.identifier lexer
pNumber = Lit <$> Tok.integer lexer

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

-----------------------------

isOperator :: SExp -> Bool
isOperator (Symbol (x:_)) = elem x opChars
isOperator _ = False

infixs =
  [(["*","/"], []),
   (["+","-"], []),
   ([], ["=","<-","::","->"])
  ]

infixsTable :: [[Operator [SExp] u Identity SExp]]
infixsTable = do
  let makeInfix assoc op = Infix ((satisfy' (== op))  >> return  (\a b -> SList [op, a, b])) assoc
  let makeItems assoc opNames = map (makeInfix assoc . Symbol) opNames

  (lefts, rights) <- infixs
  [(makeItems AssocLeft lefts) ++ (makeItems AssocRight rights)]
  

callExpr :: Parsec [SExp] s SExp
callExpr = do
  xs <- many (satisfy' (not . isOperator))
  case xs of
    [x] -> return x
    otherwise -> return (SList xs)

foldSExp' :: Parsec [SExp] s SExp
foldSExp' = buildExpressionParser infixsTable callExpr

foldSExp :: SExp -> Either ParseError SExp
foldSExp (SList []) = Right (SList [])
foldSExp (SList xs) = do
  xs' <- traverse foldSExp xs
  parse foldSExp' "<stdin>" xs'

foldSExp x = Right x


parseSExp :: String -> SExp
parseSExp s = do
  case (parse pSExp "<stdin>" s) >>= foldSExp of
    Left e  -> Symbol (show e)
    Right x -> x


code = "(f :: Int -> Int -> Int) (f a b = {d=a+b;return d})"
scode = "((f a b c) = ((d = (a+(b*c))) (return (d()))))"




