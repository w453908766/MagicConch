module AST where

import qualified Data.Map as Map
import Control.Monad.State

import Debug.Trace 

import SExp
import Parser

data MCType
  = MCInt
  | Arrow MCType MCType
  deriving (Show, Read, Eq)

data Declare = Declare String MCType 
  deriving (Show, Read, Eq)

data Pattern
  = Param Declare 
  | Pattern [Pattern]
  deriving (Show, Read, Eq)

data Stmt
  = Value Declare
  | Lit Int
  | Lambda Pattern Stmt
  | Return Stmt
  | Call [Stmt]
  | Define Declare Stmt
  | IfStmt Stmt Stmt (Maybe Stmt)
  | Block [Stmt]
  deriving (Show, Read, Eq)


type Env = Map.Map String Declare

genMCType :: SExp -> MCType
genMCType (Symbol "Int") = MCInt
genMCType (SList [ty0, Operator "->", ty1]) =
  Arrow (genMCType ty0) (genMCType ty1)

genDeclare :: SExp -> State Env Declare
genDeclare (SList [Symbol name, Operator "::", ty]) = do
  let decl = Declare name (genMCType ty)
  modify (Map.insert name decl)
  return decl

genDefine :: SExp -> State Env Stmt
genDefine (SList [Symbol name, Operator "=", expr]) = do
  decl <- gets (flip (Map.!) name)
  return $ Define decl (Lit 5)

genDefine (SList [SList (Symbol name:_), Operator "=", expr]) = do
  decl <- gets (flip (Map.!) name)
  return $ Define decl (Lit 6)


genAST' :: [SExp] -> State Env [Stmt]
genAST' globals = do
  let decls = filter (infixOf "::") globals
  let defs = filter (infixOf "=") globals
  traverse genDeclare decls
  traverse genDefine defs

genAST :: String -> [Stmt]
genAST code = defs
  where 
    globals = parseSExp code
    (defs, env) = runState (genAST' globals) Map.empty
  


  
  


 
