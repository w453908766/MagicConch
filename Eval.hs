
module Eval where

import Control.Monad.State
import Control.Applicative
import Control.Monad.Except

import Data.Map as Map
import Data.List as List
import Data.IORef

import Debug.Trace

import Syntax


data Value 
 = VInt Integer
 | VBool Bool
 | VChar Char
 | VCons String [IORef Value]
 | VClosure String Expr (IORef Env)
 | VPrim Int [Value] ([Value] -> IO Value)


instance Show Value where
  show (VInt x)  = show x
  show (VBool x) = show x
  show (VChar x) = show x
--  show (VCons name values) = "(" ++ (unwords (name : (fmap show values))) ++ ")"
  show (VClosure x body env) = "<<closure>>"
  show (VPrim n argv f) = "<<prim>>"


instance Eq Value where
  (VInt x)  == (VInt y)  = x==y
  (VBool x) == (VBool y) = x==y
  (VChar x) == (VChar y) = x==y
  (VCons n0 es0) == (VCons n1 es1) = n0==n1 && es0==es1


type Env = Map String Value
type EvalState = (ExceptT String) IO

eval :: IORef Env -> Expr -> EvalState Value
eval _ (Lit x) = 
 return $ case x of
  (IntLit x) -> VInt x
  (BoolLit x) -> VBool x
  (CharLit x) -> VChar x


eval menv (Var x) = do
  env <- lift $ readIORef menv
  case Map.lookup x env of
    Just v -> return v
    Nothing -> throwError ("Variable not in scope: " ++ (show (x,env)))

eval menv (VCtor x) = do
  env <- lift $ readIORef menv
  case Map.lookup x env of
    Just v -> return v
    Nothing -> throwError ("Variable not in scope: " ++ (show (x,env)))

eval menv (Cond cond tr fl) = do
  vc <- eval menv cond
  case vc of
    VBool True ->  eval menv tr
    VBool False -> eval menv fl
    _ -> throwError "Condition is not Bool Type"

eval menv (Block es) = do
  rs <- traverse (eval menv) es
  return $ last rs

eval menv (Case expr ps) = do
  value <- eval menv expr
  evalPatterns menv ps value

eval menv (App func arg) = do
  v <- eval menv func
  argv <- eval menv arg
  case v of
    (VClosure x body mclos) -> do
      clos <- lift $ readIORef mclos
      menv' <- lift $ newIORef $ Map.insert x argv clos
      eval menv' body

    (VPrim 1 args f) -> 
      lift $ f (reverse $ argv:args)

    (VPrim n args f) ->
      return $ VPrim (n-1) (argv:args) f

    (VCons ctor elems) -> do
      x <- lift $ newIORef argv
      return $ VCons ctor (elems++[x])
       
    _ -> throwError ((show v) ++ " is not a function")

eval menv (Lambda x body) = do
  return $ VClosure x body menv


eval menv (Let decls) = do
  evalDecls menv decls
  return $ VBool True

-----------------------------------------

 
evalDecl :: IORef Env -> Decl -> EvalState ()
evalDecl menv (Val pat expr) = do
  value <- eval menv expr
  match menv pat value

evalDecl menv (Func name params body) = do
  let body' = List.foldr Lambda body params
  clos <- eval menv body'
  env <- lift $ readIORef menv
  lift $ writeIORef menv (Map.insert name clos env)


evalDecl menv (TypeCtor _ _ ctors) = do
  let f (name, _) = Map.insert name (VCons name [])
  env <- lift $ readIORef menv
  let env' = List.foldr f env ctors
  lift $ writeIORef menv env'
  
  
evalDecls :: IORef Env -> [Decl] -> EvalState ()
evalDecls menv decls = do
  traverse (evalDecl menv) decls
  return ()


evalModule :: Module -> IO (Either String Env)
evalModule (Module decls) = do
  menv <- newIORef initEnv
  ret <- runExceptT (evalDecls menv decls)

  case ret of
    Right _ -> Right <$> readIORef menv
    Left err -> return $ Left err



-----------------------

matchLit :: Lit -> Value -> Bool
matchLit (IntLit x)  (VInt y)  = x==y
matchLit (BoolLit x) (VBool y) = x==y
matchLit (CharLit x) (VChar y) = x==y
     
match :: IORef Env -> Pattern -> Value -> EvalState ()
match menv PWild _ = return ()

match menv (PLit x) y = do
  if matchLit x y 
  then return ()
  else throwError "Dismatch Lit"
  

match menv (PCons pctor ps) (VCons vctor mvs) = do
  if pctor == vctor 
  then do 
    vs <- lift $ traverse readIORef mvs
    matchs menv ps vs
  else throwError "Dismatch Ctor"

  
match menv (PVar capture name mpat) value = do
  case mpat of
    Nothing -> return ()
    (Just pat) -> match menv pat value

  env' <- lift $ readIORef menv

  if capture
  then case Map.lookup name env' of
    Nothing -> throwError ("Variable not in scope: " ++ (show (name,env')))
    Just v -> 
      if v==value 
      then return ()
      else throwError "Can not match capture variable"
  else lift $ writeIORef menv $ Map.insert name value env'
  
matchs menv [] [] = return ()
matchs menv _ [] = throwError "Dismatch Ctor"
matchs menv [] _ = throwError "Dismatch Ctor"
matchs menv (p:ps) (v:vs) = do
  match menv p v
  matchs menv ps vs
 
evalPatterns :: IORef Env -> Cases -> Value -> EvalState Value
evalPatterns menv [] _ = throwError "Can not match any pattern"
evalPatterns menv ((pat,body):ps) value = do
  env <- lift $ readIORef menv
  let f = do
       menv' <- lift $ newIORef env
       match menv' pat value
       eval menv' body
  f <|> evalPatterns menv ps value

------------------------------------



add [VInt a, VInt b] = return $ VInt (a+b)
sub [VInt a, VInt b] = return $ VInt (a-b)
mul [VInt a, VInt b] = return $ VInt (a*b)
eq [a, b] = return $ VBool (a==b)

print0 [a] = do
  print a
  return $ VBool True

binop sym f = (sym, VPrim 2 [] f)

initEnv = fromList [
  binop "+" add, 
  binop "-" sub,
  binop "*" mul,
  binop "==" eq,
  ("print", VPrim 1 [] print0)
 ]
  

