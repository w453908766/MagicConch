{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Data.Functor.Identity
import Control.Applicative ((<$>), some)
import Data.Text.Lazy (Text, unlines)

import Text.Parsec as P hiding (char)
import Text.Parsec.Prim as Prim
import Text.Parsec.Expr as Ex
import Text.Parsec.String (Parser)
import Text.Parsec.Language (haskellDef)
import qualified Text.Parsec.Token as Tok

import Debug.Trace

import Utils
import AST
import Lexer
import ParseUtils

parseLit :: IParsec Lit
parseLit =
  (IntLit <$> intLit) <|> (CharLit <$> charLit)


---------------------------------------------------

parsePattern :: IParsec Pattern
parsePattern = do
  parseParen (PCons "Tuple") parsePattern
  <|> (reserved "_" >> return PWild)
  <|> (PLit <$> parseLit)
  <|> parsePCtor
  <|> parsePVar

parsePCtor = do
  ctor <- uppIdent
  args <- many parsePattern
  return $ PCons ctor args

parsePVar :: IParsec Pattern
parsePVar = do
  cap <- optionMaybe (char '&') 
  let captrue = cap/=Nothing
  name<-ident
  as <- optionMaybe (char '@' >> parsePattern)
  return $ PVar captrue name as


---------------------------------------------------
parseType :: IParsec Type
parseType = do
  ts <- sepBy1 parseType' (reservedOp "->")
  return $ foldr1 (\t0 t1 -> TCons "->" [t0,t1]) ts

parseType' = do
  parseParen (TCons "Tuple") parseType
  <|> TVar <$> ident
  <|> TCons <$> uppIdent <*> many parseType
  

---------------------------------------------------

parseProto :: Pattern -> IParsec Decl   
parseProto (PVar False name Nothing) = do
  reservedOp "::"
  typ <- parseType
  return $ Proto name typ 

parseProto p = trace ("Proto" ++ show p) undefined
       
parseFunc :: Pattern -> IParsec Decl
parseFunc (PVar False name Nothing) = do
  params <- some parsePattern
  reservedOp "="
  body <- parseExpr
  return $ Func name [(params, body)]
  
parseVal :: Pattern -> IParsec Decl
parseVal pat = do 
  reservedOp "="
  val <- parseExpr
  return $ Val pat val


parseDecl :: IParsec Decl
parseDecl = do
  pat <- parsePattern
  parseProto pat <|> parseVal pat <|> parseFunc pat

---------------------------------------------------

parseModule :: IParsec [Decl]
parseModule = do
  spaces
  decls <- block parseDecl
  eof
  return decls

---------------------------------------------------


parseExpr :: IParsec Expr
parseExpr = do
  traceM "parseExpr"
  parseLambda
  <|> parseCond
  <|> parseCase
  <|> parseLet 
  <|> parseBlock
  <|> parseDeRef
  <|> Ex.buildExpressionParser priority parseExpr'

parseExpr' = do
  traceM "parseExpr'"
  es <- some parseExpr''
  return $ combine (foldl1 App) es

parseExpr'' = do
  traceM "parseExpr''"
  parseParen (foldl App (VCtor "Tuple")) parseExpr
  <|> Lit <$> parseLit
  <|> Var <$> ident 
  <|> VCtor <$> uppIdent

parseDeRef = do
  char '!'
  DeRef <$> ident

parseLet = do
  reserved "let"
  Let <$> block parseDecl

parseBlock = do
  reserved "block"
  Block <$> block parseExpr
   
parseLambda = do
  char '\\'
  pats <- some parsePattern
  reservedOp "=>"
  body <- parseExpr
  return $ Lambda pats body

parseCond = do
  pos <- sourceColumn <$> getPosition
  reserved "if"
  cond <- parseExpr
  
  putState (ParseState (>=) pos)
  reserved "then"
  true' <- parseExpr
  
  putState (ParseState (>=) pos)
  reserved "else"
  false' <- parseExpr

  return $ Cond cond true' false'
  
parseCase = do
  reserved "case"
  val <- parseExpr 
  reserved "of"
  caluse <- block $ do
    pat <- parsePattern 
    reservedOp "=>"
    body <- parseExpr
    return $ (pat, body)

  return $ Case val caluse


    
 
infixOp :: String -> Ex.Assoc -> Ex.Operator Text ParseState Identity Expr
infixOp x = Ex.Infix $ do
  reservedOp x
  return (\e0 e1 -> App (App (Var x) e0) e1)

priority :: [[Ex.Operator Text ParseState Identity Expr]] 
priority = [
    [infixOp "*" Ex.AssocLeft],
    [infixOp "+" Ex.AssocLeft
    ,infixOp "-" Ex.AssocLeft
    ],
    [infixOp "==" Ex.AssocLeft],
    [infixOp "<-" Ex.AssocNone]
  ]
 
  
code = Data.Text.Lazy.unlines
 [ "fact :: Int -> Int"
 , "fact 0 = 1"
 , "fact n = block"
 , "  let ret = fact(n-1)"
 , "  n*ret"
 ]

ast = run parseModule code

code1 = "fact n = n*fact(n-1)"
test = run parseModule code1

