-- Interpreter for MyLatte language
{-# LANGUAGE ImportQualifiedPost #-}

module Interpreter (interpret) where

import Control.Exception (throw)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Map qualified as M
import Exception
import GHC.Show (Show)
import MyLatte.Abs
import Typedefs
import Utils

bindArgs :: BNFC'Position -> [Arg] -> [Expr] -> Env Loc -> InterpreterMonad (Env Loc)
bindArgs _ [] [] env = do
  (locMap, _) <- get
  return env
bindArgs pos ((Arg _ (ArgTypeVal _ t) (Ident ident)) : args) (expr : exprs) env = do
  val <- evalExpr expr
  (locMap, nextLoc) <- get
  put (M.insert nextLoc val locMap, nextLoc + 1)
  let updatedEnv = M.insert (Ident ident) nextLoc env
  bindArgs pos args exprs updatedEnv
bindArgs pos ((Arg _ (ArgTypeRef _ t) (Ident ident)) : args) ((EVar _ (Ident ident')) : exprs) fEnv = do
  env <- ask
  loc <- findVarLoc (Ident ident') pos
  let updatedEnv = M.insert (Ident ident) loc fEnv
  bindArgs pos args exprs updatedEnv
bindArgs pos _ _ _ = throwError $ InvalidPassByReference pos

interpret :: Program -> IO (Either RuntimeErr ((Env Loc, RetVal), Store))
interpret (Program _ stmts) = runExceptT $ runStateT (runReaderT (interpretStmts stmts) M.empty) (M.empty, 0)

insertItems :: Type -> [Item] -> InterpreterMonad (Env Loc, RetVal)
insertItems _ [] = do
  env <- ask
  return (env, Nothing)
insertItems t ((Init pos (Ident ident) expr) : tail) = do
  val <- evalExpr expr
  env <- ask
  (locMap, nextLoc) <- get
  put (M.insert nextLoc val locMap, nextLoc + 1)
  let updatedEnv = M.insert (Ident ident) nextLoc env
  (locMap, nextLoc) <- get
  return (updatedEnv, Nothing)
-- uninitialized variable has to be initialized before use
insertItems t ((NoInit pos (Ident ident)) : tail) = do
  env <- ask
  (locMap, nextLoc) <- get
  updatedLocMap <- case t of
    Int _ -> return $ M.insert nextLoc (Vint 0) locMap
    Bool _ -> return $ M.insert nextLoc (Vbool False) locMap
    Str _ -> return $ M.insert nextLoc (Vstr "") locMap

  put (updatedLocMap, nextLoc + 1)
  let updatedEnv = M.insert (Ident ident) nextLoc env
  return (updatedEnv, Nothing)

interpretStmts :: [Stmt] -> InterpreterMonad (Env Loc, RetVal)
interpretStmts [] = do
  env <- ask
  return (env, Nothing)
interpretStmts (stmt : stmts) = do
  (env, rV) <- interpretStmt stmt
  case rV of
    Just _ -> return (env, rV)
    Nothing -> local (const env) (interpretStmts stmts)

interpretStmt :: Stmt -> InterpreterMonad (Env Loc, RetVal)
-- function definition: add a new function to the environment
interpretStmt (FnDef pos t (Ident ident) args (Block _ stmts)) = do
  env <- ask
  (locMap, nextLoc) <- get
  let env' = M.insert (Ident ident) nextLoc env
  let store' = M.insert nextLoc (FVal t args (Block pos stmts) env') locMap
  put (store', nextLoc + 1)
  return (env', Nothing)

-- empty statement
interpretStmt (Empty _) = do
  env <- ask
  return (env, Nothing)

-- block statement
interpretStmt (BStmt _ (Block _ stmts)) = interpretStmts stmts
-- variable declaration: add a new variable to the environment
interpretStmt (Decl pos t items) = do
  (updatedEnv, rV) <- insertItems t items
  return (updatedEnv, rV)

-- assignment: update the value of a variable
interpretStmt (Ass pos (Ident ident) expr) = do
  env <- ask
  (locMap, nextLoc) <- get
  loc <- findVarLoc (Ident ident) pos
  -- let maybeLoc = M.lookup (Ident ident) env
  newVal <- evalExpr expr
  oldVal <- findVal loc pos
  put (M.insert loc newVal locMap, nextLoc)
  return (env, Nothing)

-- incrementation statement:
interpretStmt (Incr pos (Ident ident)) = do
  env <- ask
  (locMap, nextLoc) <- get
  loc <- findVarLoc (Ident ident) pos
  -- let maybeLoc = M.lookup ident env
  (Vint oldVal) <- findVal loc pos
  put (M.insert loc (Vint (oldVal + 1)) locMap, nextLoc)
  return (env, Nothing)

-- decrementation statement:
interpretStmt (Decr pos (Ident ident)) = do
  env <- ask
  (locMap, nextLoc) <- get
  loc <- findVarLoc (Ident ident) pos
  -- let maybeLoc = M.lookup ident env
  (Vint oldVal) <- findVal loc pos
  put (M.insert loc (Vint (oldVal - 1)) locMap, nextLoc)
  return (env, Nothing)

-- Return statement
interpretStmt (Ret pos expr) = do
  env <- ask
  val <- evalExpr expr
  (locMap, _) <- get
  return (env, Just val)

-- Conditional statement
interpretStmt (Cond pos expr stmt) = do
  env <- ask
  val <- evalExpr expr
  case val of
    Vbool True -> interpretStmt stmt
    Vbool False -> return (env, Nothing)

-- Conditional else statement
interpretStmt (CondElse pos expr stmt1 stmt2) = do
  env <- ask
  val <- evalExpr expr
  case val of
    Vbool True -> interpretStmt stmt1
    Vbool False -> interpretStmt stmt2

-- While loop
interpretStmt (While pos expr stmt) = do
  env <- ask
  val <- evalExpr expr
  case val of
    Vbool True -> do
      (_, rV) <- interpretStmt stmt
      case rV of
        Just _ -> return (env, rV)
        Nothing -> interpretStmt (While pos expr stmt)
    Vbool False -> return (env, Nothing)

-- Single expression statement
interpretStmt (SExp _ expr) = do
  env <- ask
  val <- evalExpr expr
  return (env, Nothing)

evalExpr :: Expr -> InterpreterMonad Val
-- variable expression:
evalExpr (EVar pos (Ident ident)) = do
  env <- ask
  (locMap, _) <- get
  loc <- findVarLoc (Ident ident) pos
  -- let maybeLoc = M.lookup ident env
  oldVal <- findVal loc pos
  return oldVal

-- integer literal expression:
evalExpr (ELitInt _ n) = return $ Vint n
-- true literal expression:
evalExpr (ELitTrue _) = return $ Vbool True
-- false literal expression:
evalExpr (ELitFalse _) = return $ Vbool False
-- print expression:
evalExpr (EApp pos (Ident "print") [expr]) = do
  val <- evalExpr expr
  liftIO $ print val
  return val
-- readInt expression:
evalExpr (EApp pos (Ident "readInt") []) = do
  n <- liftIO readLn
  return $ Vint n
-- readString expression:
evalExpr (EApp pos (Ident "readString") []) = do
  s <- liftIO getLine
  return $ Vstr s

-- function application:
evalExpr (EApp pos (Ident ident) args) = do
  env <- ask
  (locMap, nextLoc) <- get
  loc <- findVarLoc (Ident ident) pos
  (FVal rType formalArgs block fEnv) <- findVal loc pos

  updatedEnv <- bindArgs pos formalArgs args fEnv
  (_, rV) <- local (const updatedEnv) (interpretStmts [BStmt pos block])
  (locMap, _) <- get
  case rV of
    Just val -> return val
    Nothing -> throwError $ NoReturn pos

-- string literal:
evalExpr (EString _ s) = return $ Vstr s
-- arithmetic negation:
evalExpr (Neg pos expr) = do
  val <- evalExpr expr
  case val of
    Vint n -> return $ Vint (-n)
    _ -> throwError $ Other pos

-- logical negation:
evalExpr (Not pos expr) = do
  val <- evalExpr expr
  case val of
    Vbool b -> return $ Vbool (not b)
    _ -> throwError $ Other pos

-- multiplication, division, and modulo:
evalExpr (EMul pos expr1 (Times _) expr2) = do
  val1 <- evalExpr expr1
  val2 <- evalExpr expr2
  case (val1, val2) of
    (Vint n1, Vint n2) -> return $ Vint (n1 * n2)
    _ -> throwError $ Other pos
evalExpr (EMul pos expr1 (Div _) expr2) = do
  val1 <- evalExpr expr1
  val2 <- evalExpr expr2
  case (val1, val2) of
    (Vint n1, Vint 0) -> throwError $ DivisionByZero pos
    (Vint n1, Vint n2) -> return $ Vint (n1 `div` n2)
    _ -> throwError $ Other pos
evalExpr (EMul pos expr1 (Mod _) expr2) = do
  val1 <- evalExpr expr1
  val2 <- evalExpr expr2
  case (val1, val2) of
    (Vint n1, Vint 0) -> throwError $ DivisionByZero pos
    (Vint n1, Vint n2) -> return $ Vint (n1 `mod` n2)
    _ -> throwError $ Other pos

-- addition and subtraction:
evalExpr (EAdd pos expr1 (Plus _) expr2) = do
  val1 <- evalExpr expr1
  val2 <- evalExpr expr2
  case (val1, val2) of
    (Vint n1, Vint n2) -> return $ Vint (n1 + n2)
    _ -> throwError $ Other pos
evalExpr (EAdd pos expr1 (Minus _) expr2) = do
  val1 <- evalExpr expr1
  val2 <- evalExpr expr2
  case (val1, val2) of
    (Vint n1, Vint n2) -> return $ Vint (n1 - n2)
    _ -> throwError $ Other pos

-- relational operators:
evalExpr (ERel pos expr1 op expr2) = do
  val1 <- evalExpr expr1
  val2 <- evalExpr expr2
  case op of
    LTH _ -> return $ Vbool (val1 < val2)
    LE _ -> return $ Vbool (val1 <= val2)
    GTH _ -> return $ Vbool (val1 > val2)
    GE _ -> return $ Vbool (val1 >= val2)
    EQU _ -> return $ Vbool (val1 == val2)
    NE _ -> return $ Vbool (val1 /= val2)

-- logical and:
evalExpr (EAnd pos expr1 expr2) = do
  val1 <- evalExpr expr1
  case val1 of
    Vbool False -> return $ Vbool False
    Vbool True -> do
      val2 <- evalExpr expr2
      case val2 of
        Vbool b -> return $ Vbool b
        _ -> throwError $ Other pos
    _ -> throwError $ Other pos

-- logical or:
evalExpr (EOr pos expr1 expr2) = do
  val1 <- evalExpr expr1
  case val1 of
    Vbool True -> return $ Vbool True
    Vbool False -> do
      val2 <- evalExpr expr2
      case val2 of
        Vbool b -> return $ Vbool b
        _ -> throwError $ Other pos
    _ -> throwError $ Other pos

-- Concatenation:
evalExpr (Concat pos expr1 expr2) = do
  val1 <- evalExpr expr1
  val2 <- evalExpr expr2
  case (val1, val2) of
    (Vstr s1, Vstr s2) -> return $ Vstr (s1 ++ s2)
    _ -> throwError $ Other pos

-- TODO: Lambda expression
evalExpr (LExpr pos rType args block) = do
  env <- ask
  return $ FVal rType args block env

-- Lambda application
evalExpr (LApp pos rType args block exprs) = do
  env <- ask
  (locMap, nextLoc) <- get

  updatedEnv <- bindArgs pos args exprs env
  (_, rV) <- local (const updatedEnv) (interpretStmts [BStmt pos block])
  case rV of
    Just val -> return val
    Nothing -> throwError $ NoReturn pos