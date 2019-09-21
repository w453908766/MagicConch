{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Control.Applicative ((<$>), some)
import Data.Text.Lazy (Text, unlines)

import Text.Parsec as P
import Text.Parsec.Prim as Prim
import Text.Parsec.Expr
import Text.Parsec.String (Parser)
import Text.Parsec.Language (haskellDef)
import qualified Text.Parsec.Token as Tok

import Debug.Trace

import Helper
import Lexer
import AST

parseLit :: IParsec Lit
parseLit =
  (IntLit <$> intLit) <|> (CharLit <$> charLit)

---------------------------------------------------

parsePattern :: IParsec Pattern
parsePattern = do
  parens parsePattern
  <|> parsePWild
  <|> (PLit <$> parseLit)
  <|> parsePCtor
  <|> (ident >>= parsePattern0)

parsePWild = do
  reserved "_"
  return PWild

parsePCtor = do
  ctor <- uppIdent
  args <- many parsePattern
  return $ PCtor ctor args

parsePattern0 :: String -> IParsec Pattern
parsePattern0 name = (
  do
    sat (=='@')
    p <- parsePattern
    return $ PAspat name p
  ) <|> (return $ PVar name)

---------------------------------------------------
parseType :: IParsec Typ
parseType = do
  ts <- sepBy1 uppIdent (reservedOp "->")
  return $ foldr1 TApp (fmap TVar ts)

---------------------------------------------------

parseProto :: Pattern -> IParsec Decl   
parseProto (PVar name) = do
  reservedOp "::"
  typ <- parseType
  return $ Proto name typ 
  
parseProtoFamily :: String -> [Pattern] -> IParsec Decl
parseProtoFamily name pats = do
  reservedOp "::"
  typ <- parseType
  let params = [x|(PVar x)<-pats]
  return $ ProtoFamily name params typ
  
       
parseFunc :: String -> [Pattern] -> IParsec Decl
parseFunc name pats = do
  reservedOp "="
  body <- some parseExpr
  return $ Func name pats body
  
parseVal :: Pattern -> IParsec Decl
parseVal pat = do 
  reservedOp "="
  val <- parseExpr
  return $ Val pat val


parseDecl :: IParsec Decl
parseDecl = do
  pats <- some parsePattern
  case pats of
    [pat] ->
      parseProto pat
      <|> parseVal pat
    (PVar name:params) -> 
      parseProtoFamily name params
      <|> parseFunc name params
  
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
  es <- some parseExpr'
  return $ case es of
    [e] -> e
    es -> foldl1 App es
   
  

parseExpr' :: IParsec Expr
parseExpr' = do
  (parens parseExpr)
  <|> (Lit <$> parseLit)
  <|> (Ctor <$> uppIdent)
  <|> parseLambda
  <|> parseCond
  <|> parseCase
  <|> parseReturn
  <|> parseVar

parseVar :: IParsec Expr
parseVar = do
  var <- ident
  (do
    reservedOp "="
    expr <- parseExpr
    return $ Let var expr
   ) <|> (return $ Var var)
    
  

parseReturn :: IParsec Expr
parseReturn = do
  reserved "return"
  Return <$> parseExpr

parseLambda :: IParsec Expr
parseLambda = do
  sat (=='\\')
  pats <- some parsePattern
  reservedOp "=>"
  body <- many parseExpr
  return $ Lambda pats body

parseCond :: IParsec Expr
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
  
parseCase :: IParsec Expr
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
    
  
  
  
   



code = Data.Text.Lazy.unlines
 [ "fact :: Arrow Int Int"
 , "fact 0 = 1"
 , "fact n ="
 , " 5"
 , " 6"
 , " 7"
 ]

ast = run parseModule code

