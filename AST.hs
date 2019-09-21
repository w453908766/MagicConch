module AST where


data Pattern
  = PVar String              -- x
  | PCtor String [Pattern]   -- C1 t1 t2
  | PAspat String Pattern    -- x@p
  | PLit Lit                 -- 5 or 'c'
  | PWild                    -- _
  deriving (Show, Read, Eq)

data Expr
  = Var String               -- x
  | Lit Lit                  -- 4 or 'c'
  | Ctor String              -- T1
  | App Expr Expr            -- f x
  | Lambda [Pattern] StmtBlock    -- \p1 p2 -> e
  | Cond Expr Expr Expr      -- if e1 then e2 else e3
  | Case Expr [(Pattern, Expr)]       -- case xxx of a => stmt1 ; b => stmt2
  | Return Expr              -- return x
  | Let String Expr       -- x = expr
  deriving (Show, Read, Eq)

type StmtBlock = [Expr]

data Decl
  = Proto String Typ         -- f :: a
  | ProtoFamily String [String] Typ -- f a b :: a -> b -> b
  | Func String [Pattern] StmtBlock       -- f a b = body
  | Val Pattern Expr         -- a = x
  deriving (Show, Read, Eq)

data Typ
  = TVar String              -- a
  | TCtor String             -- T
  | TApp Typ Typ             -- T a
  deriving (Show, Read, Eq)

data Lit
  = IntLit Integer
  | CharLit Char
  deriving (Show, Read, Eq)
  


