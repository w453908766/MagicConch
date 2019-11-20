{-# LANGUAGE FlexibleContexts #-}

module ParseUtils where

import Data.Text.Lazy

import Text.Parsec
import Text.Parsec.Pos
import Text.Parsec.Prim hiding (State)

import Control.Monad.Identity
import Control.Applicative ((<$>), some)

import Debug.Trace

import Utils

-- Indentation sensitive Parsec monad.
type IParsec a = Parsec Text ParseState a

data ParseState = ParseState
  { 
    cmp :: Column -> Column -> Bool,
    indents :: Column
  }



initParseState :: ParseState
initParseState = ParseState (==) 1

indented :: IParsec ()
indented = do
  current <- sourceColumn <$> getPosition
  (ParseState cmp col) <- getState
  guard (cmp current col)
  putState (ParseState (>) col)
  
block :: IParsec a -> IParsec [a]
block p = do
  state <- getState
  cur <- sourceColumn <$> getPosition
  res <- Control.Applicative.some (putState (ParseState (==) cur) >> p)
  putState state
  return res


run :: IParsec a -> Text -> Either ParseError a
run p = runParser p initParseState "<file>"


sat p = indented >> satisfy p >> spaces


parseParen :: ([a]->a) -> IParsec a -> IParsec a
parseParen f p = do
  ps <- parseBrackets '(' ')' p
  return $ combine f ps

parseBrackets :: Char -> Char -> IParsec a -> IParsec [a]
parseBrackets start end p = do
  sat (== start)
  ps <- sepBy1 p (sat (== ','))
  sat (== end)
  return ps
  

