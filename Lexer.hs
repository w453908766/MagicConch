
{-# LANGUAGE OverloadedStrings #-}

module Lexer where

import Data.Char
import Data.String

import Text.Parsec as P
import Text.Parsec.Prim as Prim
import Text.Parsec.Expr
import Text.Parsec.String (Parser)
import Text.Parsec.Language (haskellDef)
import qualified Text.Parsec.Token as Tok

import ParseUtils

opChars = ":!#$%&*+./<=>?@\\^|-~"

langDef = Tok.LanguageDef
  { Tok.commentStart    = "{-"
  , Tok.commentEnd      = "-}"
  , Tok.commentLine     = "--"
  , Tok.nestedComments  = True
  , Tok.identStart      = lower
  , Tok.identLetter     = alphaNum <|> oneOf "_'"
  , Tok.opStart         = oneOf opChars
  , Tok.opLetter        = oneOf opChars
  , Tok.reservedNames   = ["block", "case","of","if","then","else","data","type","import","module","return","_", "where", "Ref"]
--  , Tok.reservedOpNames = ["::","=","<-","@", ";", "+", "-", "*", "/", "%"]
  , Tok.reservedOpNames = ["::","..","=","\\","|","->","@","~","=>"]
  , Tok.caseSensitive   = True
  }


lexer = Tok.makeTokenParser langDef

uppIdent0 = Tok.lexeme lexer $ ((do
  c <- upper
  cs <- many (Tok.identLetter langDef)
  return (c:cs)
  ) <?> "UppIdent")

----------------------------------------
operator = indented >> Tok.operator lexer
reservedOp r = indented >> Tok.reservedOp lexer r

ident = indented >> Tok.identifier lexer
reserved r = indented >> Tok.reserved lexer r

uppIdent = indented >> uppIdent0

intLit = indented >> Tok.natural lexer
charLit = indented >> Tok.charLiteral lexer
boolLit = do
  indented
  (reserved "True" >> return True)
   <|> (reserved "False" >> return False)

braces p = indented >> Tok.braces lexer p
parens p = indented >> Tok.parens lexer p

char c = sat (==c)


{-
Tok.comma            Tok.semiSep1
Tok.brackets         Tok.commaSep         Tok.decimal              Tok.natural                Tok.squares
Tok.commaSep1        Tok.dot            Tok.naturalOrFloat     Tok.stringLiteral
Tok.float                 Tok.semi             Tok.symbol
Tok.angles           Tok.colon            Tok.lexeme           Tok.octal                 Tok.semiSep          Tok.whiteSpace
-}
