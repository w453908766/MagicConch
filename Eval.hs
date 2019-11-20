
module Eval where

import Control.Monad.Except
import Control.Applicative
import Text.Printf

import Data.Map as Map
import Data.IORef

import Syntax


data Value 
 = VInt Integer
 | VBool Bool
 | VChar Char
 | VRef (IORef Value)
 | VCons String [Value]
 | VClosure String Expr (IORef Env)
 | VPrim Int [Value] ([Value] -> IO Value)


instance Show Value where
  show (VInt x)  = show x
  show (VBool x) = show x
  show (VChar x) = show x
  show (VRef _) = "<<ref>>"
  show (VCons name values) = printf "(%s %s)" name (unwords $ fmap show values)
  show (VClosure x body env) = "<<closure>>"
  show (VPrim n argv f) = "<<prim>>"


instance Eq Value where
  (VInt x)  == (VInt y)  = x==y
  (VBool x) == (VBool y) = x==y
  (VChar x) == (VChar y) = x==y
  (VRef x) == (VRef y) = x==y
  (VCons n0 es0) == (VCons n1 es1) = n0==n1 && es0==es1


type Env = Map String Value

eval :: IORef Env -> Expr -> IO Value
eval _ (Lit x) = 
 return $ case x of
  (LInt x) -> VInt x
  (LBool x) -> VBool x
  (LChar x) -> VChar x


eval menv (Var x) = do
  env <- readIORef menv
  case Map.lookup x env of
    Just v -> return v
    Nothing -> fail $ printf "Variable not in scope: %s" (show x)

eval menv (DeRef x) = do
  ref <- eval menv x
  case ref of
    VRef r -> readIORef r
    _ -> fail $ printf "dereference a unknown value: %s" ++ (show x)

eval menv (Cond cond tr fl) = do
  vc <- eval menv cond
  case vc of
    VBool True ->  eval menv tr
    VBool False -> eval menv fl
    _ -> fail $ printf "Condition is not Bool Type: %s" (show vc)

eval menv (Block es) = do
  rs <- traverse (eval menv) es
  return $ last rs

eval menv (Case expr ps) = do
  value <- eval menv expr
  res <- runExceptT $ evalPatterns menv ps value
  case res of
    Left err -> fail err
    Right ret -> return ret

eval menv (App func arg) = do
  v <- eval menv func
  argv <- eval menv arg
  case v of
    (VClosure x body mclos) -> do
      clos <- readIORef mclos
      menv' <- newIORef $ Map.insert x argv clos
      eval menv' body

    (VPrim 1 args f) -> 
      f (reverse $ argv:args)

    (VPrim n args f) ->
      return $ VPrim (n-1) (argv:args) f

    (VCons ctor elems) -> do
      return $ VCons ctor (elems++[argv])
       
    _ -> fail $ printf "%s is not a function" (show v)

eval menv (Lambda x body) = do
  return $ VClosure x body menv


eval menv (Let decls) = do
  evalDecls menv decls
  return $ VCons "Tuple" []

-----------------------------------------

 
evalDecl :: IORef Env -> Decl -> IO ()
evalDecl menv (Val pat expr) = do
  value <- eval menv expr
  res <- runExceptT $ match menv pat value
  case res of
    Left err -> fail err
    Right () -> return ()

evalDecl menv (Func name params body) = do
  let body' = Prelude.foldr Lambda body params
  clos <- eval menv body'
  env <- readIORef menv
  writeIORef menv (Map.insert name clos env)


evalDecl menv (TypeCtor _ _ ctors) = do
  let f (name, _) = Map.insert name (VCons name [])
  env <- readIORef menv
  let env' = Prelude.foldr f env ctors
  writeIORef menv env'

evalDecl menv (TypeAlias _ _ _) = do
  return ()

evalDecl menv (Proto _ _) = do
  return ()
  
  
evalDecls :: IORef Env -> [Decl] -> IO ()
evalDecls menv decls = do
  traverse (evalDecl menv) decls
  return ()


evalModule :: Module -> IO Env
evalModule (Module decls) = do
  menv <- newIORef initEnv
  evalDecls menv decls
  readIORef menv



-----------------------
     
match :: IORef Env -> Pattern -> Value -> ExceptT String IO ()
match menv PWild _ = return ()

match menv (PLit xx) yy = do
  case (xx,yy) of
    (LInt x, VInt y)  -> guard $ x==y
    (LBool x, VBool y) -> guard $ x==y
    (LChar x, VChar y) -> guard $ x==y
    _ -> fail $ printf "Dismatch Type: %s & %s" (show xx) (show yy)
  
match menv (PCons "Ref" [pat]) (VRef ref) = do
  v <- lift $ readIORef ref 
  match menv pat v

match menv (PCons "Ref" pats) (VRef ref) = do
  fail $ printf "Ref can not have multiple value: Ref %s" (unwords $ fmap show pats)

match menv (PCons pctor ps) (VCons vctor vs) = do
  if pctor == vctor 
  then matchs menv ps vs
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
matchs menv _ [] = fail "Dismatch Ctor"
matchs menv [] _ = fail "Dismatch Ctor"
matchs menv (p:ps) (v:vs) = do
  match menv p v
  matchs menv ps vs
 
evalPatterns :: IORef Env -> Cases -> Value -> ExceptT String IO Value
evalPatterns menv [] _ = throwError "Can not match any pattern"
evalPatterns menv ((pat,body):ps) value = do
  env <- lift $ readIORef menv
  let f = do
       menv' <- lift $ newIORef env
       match menv' pat value
       lift $ eval menv' body
  f <|> evalPatterns menv ps value

------------------------------------



add [VInt a, VInt b] = return $ VInt (a+b)
sub [VInt a, VInt b] = return $ VInt (a-b)
mul [VInt a, VInt b] = return $ VInt (a*b)
eq [a, b] = return $ VBool (a==b)

newRef [x] = do
  r <- newIORef x
  return $ VRef r

write [VRef p, x] = do
  writeIORef p x
  return $ VBool True

print0 [a] = do
  print a
  return $ VBool True

binop sym f = (sym, VPrim 2 [] f)

initEnv = fromList [
  binop "+" add, 
  binop "-" sub,
  binop "*" mul,
  binop "==" eq,
  binop "<-" write,
  ("print", VPrim 1 [] print0),
  ("Ref", VPrim 1 [] newRef)
 ]
  

