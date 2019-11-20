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
import Syntax
import Lexer
import ParseUtils

parseLit :: IParsec Lit
parseLit =
  (LInt <$> intLit) 
   <|> (LChar <$> charLit)
   <|> (LBool <$> boolLit)


---------------------------------------------------

parsePattern :: IParsec Pattern
parsePattern = do
  parseParen (PCons "Tuple") parsePattern
  <|> (reserved "_" >> return PWild)
  <|> (PLit <$> parseLit)
  <|> parsePRef
  <|> parsePCtor
  <|> parsePVar

parsePRef = do
  reserved "Ref"
  p <- parsePattern
  return $ PCons "PCons" [p]

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

parseFunc (PVar False name Nothing) = do
  params <- some ident
  reservedOp "="
  body <- parseExpr
  return $ Func name params body
  
parseVal pat = do 
  reservedOp "="
  val <- parseExpr
  return $ Val pat val

parseProto :: Pattern -> IParsec Decl   
parseProto (PVar False name Nothing) = do
  reservedOp "::"
  typ <- parseType
  return $ Proto name typ 
       
parseTypeAlias :: IParsec Decl
parseTypeAlias = do
  reserved "type"
  name <- uppIdent
  params <- many ident
  reservedOp "="
  typ <- parseType
  return $ TypeAlias name params typ
  
parseTypeCtor = do
  reserved "data"
  tyName <- uppIdent
  params <- many ident
  reserved "where"
  ctors <- block $ do
    vName <- uppIdent
    tparams <- many parseType
    return (vName, tparams)
  return $ TypeCtor tyName params ctors


parseDecl :: IParsec Decl
parseDecl = do
  parseTypeAlias 
   <|> parseTypeCtor
   <|> do
    pat <- parsePattern
    parseProto pat 
     <|> parseVal pat
     <|> parseFunc pat

---------------------------------------------------

parseModule :: IParsec Module
parseModule = do
  spaces
  decls <- block parseDecl
  eof
  return $ Module decls

{-
desugarDecls :: [Decl] -> [Decl]
desugarDecls decls = desugarDecls' decls' [] 
 where
  decls' = reverse decls
  desugarDecls' [] ds1 = ds1
  desugarDecls' ((Func name0 cases0):ds0) ((Func name1 case1):ds1)
   |name0 == name1 = desugarDecls' ds0 ((Func name0 (cases0++case1)):ds1)
  desugarDecls' (d:ds0) ds1 = desugarDecls' ds0 (d:ds1)
-}


---------------------------------------------------


parseExpr :: IParsec Expr
parseExpr = do
  parseLambda
  <|> parseCond
  <|> parseCase
  <|> parseLet 
  <|> parseBlock
  <|> parseDeRef
  <|> Ex.buildExpressionParser priority parseExpr'

parseExpr' = do
  es <- some parseExpr''
  return $ combine (foldl1 App) es

parseExpr'' = do
  parseParen (foldl App (Var "Tuple")) parseExpr
  <|> Lit <$> parseLit
  <|> Var <$> (ident <|> uppIdent)

parseDeRef = do
  char '!'
  DeRef <$> parseExpr

parseLet = do
  reserved "let"
  decls <- block parseDecl
  return $ Let decls

parseBlock = do
  reserved "block"
  Block <$> block parseExpr
   
parseLambda = do
  char '\\'
  pats <- some ident
  reservedOp "=>"
  body <- parseExpr
  return $ foldr Lambda body pats

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
 [ "a = 1"
 , "fact 0 = 1"
 , "fact n = n*fact(n-1)"
 ]

ast = run parseModule code

code1 = "fact n = n*fact(n-1)"
test = run parseModule code1

