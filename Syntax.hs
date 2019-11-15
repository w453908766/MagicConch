module Syntax where

data Lit
  = IntLit Integer
  | BoolLit Bool
  | CharLit Char
  deriving (Show, Read, Eq)

data Type
  = TVar String              -- a
  | TCons String [Type]             -- T
  deriving (Show, Read, Eq)

data Pattern
  = PVar Bool String (Maybe Pattern) -- &x@p
  | PCons String [Pattern]   -- C1 t1 t2
  | PRef Pattern             -- Ref 5
  | PLit Lit                 -- 5 or 'c'
  | PWild                    -- _
  deriving (Show, Read, Eq)

type Cases = [(Pattern, Expr)]

data Expr
  = Lit Lit                  -- 4 or 'c'

  | Var String               -- x
  | App Expr Expr            -- f x
  | DeRef Expr              -- !x

  | Lambda String Expr    -- \p1 => e
  | Cond Expr Expr Expr      -- if e1 then e2 else e3
  | Case Expr Cases       -- case xxx of a => stmt1 ; b => stmt2
  | Let [Decl]                  -- let a = x \n b = y
  | Block [Expr]              -- block [...]
  deriving (Show, Read, Eq)

data Decl
  = TypeAlias String [String] Type
  | TypeCtor String [String] [(String, [Type])]
  | Proto String Type        -- f :: a
  | Func String [String] Expr        -- f a b = body
  | Val Pattern Expr         -- a = x
  deriving (Show, Read, Eq)
    
data Module = Module [Decl]
  deriving (Show, Read, Eq)

