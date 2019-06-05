{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}

import Data.Functor.Classes
import Data.Char
import Data.String
import Data.List as List
import Data.Map as Map
import Text.Parsec as P
import Text.Parsec.Pos as Pos
import Text.Parsec.String (Parser)
import Text.Parsec.Language (haskellDef)
import Text.Parsec.Expr

import qualified Text.Parsec.Token as Tok

import Debug.Trace


m :: Maybe Int
m = do

  a <- fmap f (Just 5)
  let f = (+1)
  return a
