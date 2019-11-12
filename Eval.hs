
module Eval where

import Control.Monad.Trans.Maybe
import Control.Monad.State
import Control.Applicative


import Data.Map as Map
import Data.List as List
import Debug.Trace

import Syntax


data Value 
 = VInt Integer
 | VBool Bool
 | VChar Char
 | VCons String [Value]
 | VClosure String Expr Env
 | VPrim Int [Value] ([Value]->Value)

type Env = Map String Value

instance Show Value where
  show (VInt x) = show x
  show (VBool x) = show x
  show (VChar x) = show x
  show (VCons name values) = "VCons"
  show (VClosure x body env) = "<<closure>>"
  show (VPrim n argv f) = "<<prim>>"

instance Eq Value where
  (VInt x)  == (VInt y)  = x==y
  (VBool x) == (VBool y) = x==y
  (VChar x) == (VChar y) = x==y
  (VCons n0 es0) == (VCons n1 es1) = n0==n1 && es0==es1



eval :: Expr -> (StateT Env) (Either String) Value
eval (Lit x) = 
 return $ case x of
  (IntLit x) -> VInt x
  (BoolLit x) -> VBool x
  (CharLit x) -> VChar x


eval (Var x) = do
  env <- get
  case Map.lookup x env of
    Just v -> return v
    Nothing -> lift $ Left ("Variable not in scope: " ++ (show (x,env)))

eval (VCtor x) = do
  env <- get
  case Map.lookup x env of
    Just v -> return v
    Nothing -> lift $ Left ("Variable not in scope: " ++ (show (x,env)))

eval (Cond cond tr fl) = do
  vc <- eval cond
  case vc of
    VBool True -> eval tr
    VBool False -> eval fl
    _ -> lift $ Left ("Condition is not Bool Type")



eval (Block es) = do
  rs <- traverse eval es
  return $ last rs

eval (Case expr ps) = do
  value <- eval expr
  evalPatterns ps value

eval (App func arg) = do
  v <- eval func
  argv <- eval arg
  case v of
    (VClosure x body clos) -> 
      evalEnv (Map.insert x argv clos) body

    (VPrim 1 args f) -> 
      return $ f (reverse $ argv:args)

    (VPrim n args f) ->
      return $ VPrim (n-1) (argv:args) f
       
    _ -> lift $ Left ((show v) ++ "is not a function")

eval (Lambda x body) = do
  env <- get
  return $ VClosure x body env

eval (Let decls) = do
  evalDecls decls
  return $ VBool True

  
evalEnv :: Env -> Expr -> (StateT Env) (Either String) Value
evalEnv env' body = do
  env <- get
  put env'
  ret <- eval body
  put env
  return ret

matchLit :: Lit -> Value -> Bool
matchLit (IntLit x)  (VInt y)  = x==y
matchLit (BoolLit x) (VBool y) = x==y
matchLit (CharLit x) (VChar y) = x==y
     
match :: Pattern -> Value -> Env -> Either String Env
match PWild _ env = return env

match (PLit x) y env = do
  if matchLit x y 
  then return env
  else Left "Dismatch Lit"
  
match (PCons pctor ps) (VCons vctor vs) env = do
  if pctor == vctor 
  then matchs ps vs env
  else Left "Dismatch Ctor"
  
match (PVar capture name mpat) value env = do
  env' <- case mpat of
    Nothing -> return env
    (Just pat) -> match pat value env

  if capture
  then case Map.lookup name env' of
    Nothing -> Left ("Variable not in scope: " ++ (show (name,env')))
    Just v -> 
      if v==value 
      then return env'
      else Left "Can not match capture variable"
  else return $ Map.insert name value env'
  
matchs :: [Pattern] -> [Value] -> Env -> Either String Env
matchs [] [] env = return env
matchs (p:ps) (v:vs) env = do
  env' <- match p v env
  matchs ps vs env'


evalPatterns :: Cases -> Value -> (StateT Env) (Either String) Value
evalPatterns [] _ = lift $ Left "Can not match any pattern"
evalPatterns ((pat,body):ps) value = do
  env <- get
  case match pat value env of
    Left _ -> evalPatterns ps value
    Right env' -> evalEnv env' body



evalDecl :: Decl -> (StateT Env) (Either String) ()
evalDecl (Val pat expr) = do
  env <- get
  value <- eval expr
  case match pat value env of
    Right env' -> put env' 
    Left err -> lift $ Left err

evalDecl (Func name params body) = do
  let body' = List.foldr Lambda body params
  clos <- eval body'
  modify $ Map.insert name clos
  
evalDecls :: [Decl] -> (StateT Env) (Either String) ()
evalDecls decls = do
  let 
    isFunc (Func{}) = True
    isFunc _ = False
    isVal (Val{}) = True
    isVal _ = False
    funcs = List.filter isFunc decls
    vals = List.filter isVal decls
  traverse evalDecl funcs
  traverse evalDecl decls
  return ()

evalModule (Module decls) =
  runStateT (evalDecls decls) initEnv



add [VInt a, VInt b] = VInt (a+b)
sub [VInt a, VInt b] = VInt (a-b)
mul [VInt a, VInt b] = VInt (a*b)
eq [a, b] = VBool (a==b)

binop sym f = (sym, VPrim 2 [] f)

initEnv = fromList [
  binop "+" add, 
  binop "-" sub,
  binop "*" mul,
  binop "==" eq
 ]
  

