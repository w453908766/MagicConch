module Link where

import qualified Data.Map as Map
import Control.Monad.State

import Debug.Trace 

import SExp
import Parser

type Env = Map.Map String SExp

classify :: [SExp] -> ([SExp], [SExp]) -> ([SExp], [SExp]) 
classify [] xs = xs
classify ((SList (Symbol n:d:_)):ds) (decls, defs) = 
  case n of
  "::" -> (d:decls, defs)
  "="  -> (decls, d:defs)

genRefer :: SExp -> State Env SExp
genRefer def@(SList [Symbol "=", Symbol name, ty]) = do
  let ref = Refer def
  modify (Map.insert name ref)
  return ref

genDefine :: SExp -> State Env SExp
genDefine (SList [Symbol "=", Symbol name, expr]) = do
  decl <- gets (flip (Map.!) name)
  return $ Define decl (Lit 5)

genDefine (SList [Symbol "=", SList (Symbol name:_), expr]) = do
  decl <- gets (flip (Map.!) name)
  return $ Define decl (Lit 6)




genAST' :: [SExp] -> State Env [Stmt]
genAST' globals = do
  let (decls, defs) = classify globals ([], [])
  traverse genDeclare decls
  traverse genDefine defs

genAST :: String -> [Stmt]
genAST code = defs
  where 
    globals = parseSExp code
    (defs, env) = runState (genAST' globals) Map.empty
  
runEval :: SExp a -> IO (SExp a)
runEval (SList [Symbol "load", Symbol filepath]) = do
  handle  <-  openFile  "girlfriend.txt"  ReadMode
  contents  <-  hGetContents  handle
  putStr  contents
  hClose  handle
  
  
