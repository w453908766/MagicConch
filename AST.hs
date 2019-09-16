module AST where

import qualified Data.Map as Map
import Control.Monad.State

import Debug.Trace 

import SExp
import Parser

type Clause = ([Pattern], Expr)

data Pattern
  = PVar String              -- x
  | PCtor String [Pattern]   -- C1 t1 t2
  | PAspat String Pattern    -- x@p
  | PWild                    -- _
  deriving (Show, Read, Eq)

data Expr
  = Var String               -- x
  | Ctor String              -- T1 or 4 or 'c'
  | App Expr Expr            -- f x
  | Lambda [Pattern] Expr    -- \p1 p2 -> e
  | Cond Expr Expr Expr      -- if e1 then e2 else e3
  | Bind String [Clause]     -- f a b = body or a = x
  | Case Expr [Clause]       -- case xxx of a => stmt1 ; b => stmt2
  | Return Expr              -- return x
  | Block [Expr]             -- stmt1 ; stmt2
  | Decl String MCType       -- f :: Int -> Int
  | Data String              --
  deriving (Show, Read, Eq)

data MCType
  = TVar String              -- a
  | TCtor String             -- T
  | TApp [MCType]            -- T a b
  deriving (Show, Read, Eq)



