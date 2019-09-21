{-# LANGUAGE FlexibleContexts #-}

module Helper where

import Data.Text.Lazy

import Text.Parsec (ParseError)
import Text.Parsec.Pos
import Text.Parsec.Prim hiding (State)

import Control.Monad.Identity
import Control.Applicative ((<$>), some)

import Debug.Trace

-- Indentation sensitive Parsec monad.
type IParsec a = Parsec Text ParseState a

data ParseState = ParseState
  { 
    cmp :: Column -> Column -> Bool,
    indents :: Column
  }



initParseState :: ParseState
initParseState = ParseState (==) 0

indented :: IParsec ()
indented = do
  current <- sourceColumn <$> getPosition
  (ParseState cmp col) <- getState
  guard (cmp current col)
  putState (ParseState (>) col)
  
block :: IParsec a -> IParsec [a]
block p = do
  cur <- getState
  pos <- sourceColumn <$> getPosition
  res <- Control.Applicative.some (putState (ParseState (==) pos) >> p)
  putState cur
  return res


run :: IParsec a -> Text -> Either ParseError a
run p = runParser p initParseState "<file>"
