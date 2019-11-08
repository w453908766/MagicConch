module AST where

data Lit
  = IntLit Integer
  | CharLit Char
  deriving (Show, Read, Eq)

data Pattern
  = PVar Bool String (Maybe Pattern) -- &x@p
  | PCons String [Pattern]   -- C1 t1 t2
  | PLit Lit                 -- 5 or 'c'
  | PWild                    -- _
  deriving (Show, Read, Eq)

data Type
  = TVar String              -- a
  | TCons String [Type]             -- T
  deriving (Show, Read, Eq)

data Expr
  = Var String               -- x
  | Lit Lit                  -- 4 or 'c'
  | VCtor String              -- T1
  | App Expr Expr            -- f x
  | Lambda [Pattern] Expr    -- \p1 p2 => e
  | Cond Expr Expr Expr      -- if e1 then e2 else e3
  | Case Expr [(Pattern, Expr)]       -- case xxx of a => stmt1 ; b => stmt2
--  | Return Expr              -- return x
  | Let [Decl]                  -- let a = x \n b = y
  | Block [Expr]              -- block [...]
  | DeRef String              -- !x
  deriving (Show, Read, Eq)

data Decl
  = Proto String Type         -- f :: a
  | Func String [([Pattern], Expr)]       -- f a b = body
  | Val Pattern Expr         -- a = x
  deriving (Show, Read, Eq)


{-
data Global
  = Mod String [Define]
  | Infix String Int
  | Define Define
  | Decl String Type
-}
