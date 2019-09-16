module Parser where

import Data.Char
import Data.String
import Data.Functor.Identity
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Function

import Text.Parsec as P
import Text.Parsec.Prim as Prim
import Text.Parsec.Expr
import Text.Parsec.String (Parser)
import Text.Parsec.Language (haskellDef)
import qualified Text.Parsec.Token as Tok

import Debug.Trace

import Utils
import SExp

opChars = ":!#$%&*+./<=>?@\\^|-~"

langDef :: Tok.LanguageDef Int
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


lexer :: Tok.TokenParser Int
lexer = Tok.makeTokenParser langDef

pOperator, pSymbol, pNumber, pAtom, pBlock, pIf, pItem, pSExp :: Parsec String Int (SExp Int)

consumeIndex :: Parsec String Int Int
consumeIndex = do
  index <- getState
  putState (index+1)
  return index

pOperator = Symbol <$> consumeIndex <*> Tok.operator lexer
pSymbol = Symbol <$> consumeIndex <*> Tok.identifier lexer
pNumber = Lit <$> consumeIndex <*> Tok.integer lexer

pAtom = pOperator <|> pSymbol <|> pNumber


pKey key = do
  spaces
  index <- consumeIndex 
  Tok.reserved lexer key
  ret <- pSExp
  return (SList [Symbol index key, ret])

pBlock = SList <$> Tok.braces lexer (Tok.semiSep lexer pSExp)

pIf = do
  cond' <- pKey "if"
  then' <- pKey "then"
  else' <- option (SList []) (pKey "else")
  return $ mappend cond' $ mappend then' else'

pItem = (Tok.parens lexer pSExp) <|> pBlock <|> pIf <|> pKey "return" <|> pAtom

pSExp = spaces >> SList <$> sepBy pItem spaces

pAll = do
  sexp <- pSExp
  eof
  num <- getState
  return (num, sexp)

-----------------------------

isOperator :: SExp a -> Bool
isOperator (Symbol _ (x:_)) = elem x opChars
isOperator _ = False

infixs =
  [(["*","/"], []),
   (["+","-"], []),
   ([], ["=","<-","::","->"])
  ]

infixsTable :: [[Operator [SExp Int] u Identity (SExp Int)]]
infixsTable = do
  let infixOp opName = do
        op <- satisfy' (SExp.isSymbol opName)
        return (\a b -> SList [op, a, b])

  let makeItems assoc opNames = map ((flip Infix assoc) . infixOp) opNames

  (lefts, rights) <- infixs
  [(makeItems AssocLeft lefts) ++ (makeItems AssocRight rights)]
  

callExpr :: Parsec [SExp Int] s (SExp Int)
callExpr = do
  xs <- many (satisfy' (not . isOperator))
  case xs of
    [x] -> return x
    otherwise -> return (SList xs)

foldSExp' :: Parsec [SExp Int] s (SExp Int)
foldSExp' = buildExpressionParser infixsTable callExpr

foldSExp :: SExp Int -> Either ParseError (SExp Int)
foldSExp (SList xs) = do
  xs' <- traverse foldSExp xs
  parse foldSExp' "<stdin>" xs'

foldSExp x = Right x


parseSExp' :: String -> Either ParseError (Int, SExp Int)
parseSExp' code = do
  (num, sexp) <- runP pAll 1 "<stdin>" code
  sexp' <- foldSExp sexp
  return (num, sexp')


parseSExp code =
  case parseSExp' code of
    Left e  -> (0, Symbol 0 (show e))
    Right x -> x



code = "(f :: Int -> Int -> Int) (f a b = {d=a+b;return d})"
scode = "((f a b c) = ((d = (a+(b*c))) (return (d()))))"




