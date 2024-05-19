-- Interpreter for MyLatte language
{-# LANGUAGE ImportQualifiedPost #-}

module Interpreter (interpret) where

import Control.Exception (throw)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Map qualified as M
-- for debugging, import Trace
import Debug.Trace
import Debug.Trace (trace)
import Exception
import Exception (Exception (InvalidPassByReference, NoReturn, Other, VariableNotDeclared), posToLC)
import GHC.Show (Show)
import MyLatte.Abs
import MyLatte.Abs (BNFC'Position, Expr)

-- data Varg = Reference Ident | Value Ident

-- Function value keeps a list of arguments,
-- a block of statements (function body), and
-- an environment from the moment of declaration.

checkMaybe :: Maybe a -> Exception -> InterpreterMonad a
checkMaybe (Just x) _ = return x
checkMaybe Nothing e = throwError e

data Val = Vint Integer | Vbool Bool | Vstr String | FVal [Arg] Block Env

bindArgs :: BNFC'Position -> [Arg] -> [Expr] -> Env -> InterpreterMonad Env
bindArgs _ [] [] env = do
  (locMap, _) <- get
  return env
bindArgs pos ((Arg _ (ArgTypeVal _ t) (Ident ident)) : args) (expr : exprs) env = do
  val <- evalExpr expr
  (locMap, nextLoc) <- get
  put (M.insert nextLoc val locMap, nextLoc + 1)
  let updatedEnv = M.insert ident nextLoc env
  bindArgs pos args exprs updatedEnv
bindArgs pos ((Arg _ (ArgTypeRef _ t) (Ident ident)) : args) ((EVar _ (Ident ident')) : exprs) fEnv = do
  env <- ask
  loc <- checkMaybe (M.lookup ident' env) $ VariableNotDeclared ident' $ posToLC pos
  let updatedEnv = M.insert ident loc fEnv
  bindArgs pos args exprs updatedEnv
bindArgs pos _ _ _ = throwError $ InvalidPassByReference $ posToLC pos

instance Show Val where
  show (Vint n) = show n
  show (Vbool b) = show b
  show (Vstr s) = s
  show (FVal args block env) = "Function: " ++ show args ++ " " ++ show env

type Loc = Integer

type Name = String

type Env = M.Map Name Loc

type LocMap = M.Map Loc Val

-- instance Show LocMap where
--   show locMap = show $ M.toList locMap

type RetVal = Maybe Val

-- Store is a pair of a map from locations to values and the next free location
type Store = (LocMap, Loc)

type InterpreterMonad = ReaderT Env (StateT Store (ExceptT Exception IO))

interpret :: Program -> IO (Either Exception ((Env, RetVal), Store))
interpret (Program _ stmts) = runExceptT $ runStateT (runReaderT (interpretStmts stmts) M.empty) (M.empty, 0)

insertItems :: Type -> [Item] -> InterpreterMonad (Env, RetVal)
insertItems _ [] = do
  env <- ask
  return (env, Nothing)
insertItems t ((Init pos (Ident ident) expr) : tail) = do
  val <- evalExpr expr
  env <- ask
  (locMap, nextLoc) <- get
  put (M.insert nextLoc val locMap, nextLoc + 1)
  let updatedEnv = M.insert ident nextLoc env
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
  let updatedEnv = M.insert ident nextLoc env
  return (updatedEnv, Nothing)

interpretStmts :: [Stmt] -> InterpreterMonad (Env, RetVal)
interpretStmts [] = do
  env <- ask
  return (env, Nothing)
interpretStmts (stmt : stmts) = do
  (env, rV) <- interpretStmt stmt
  case rV of
    Just _ -> return (env, rV)
    Nothing -> local (const env) (interpretStmts stmts)

interpretStmt :: Stmt -> InterpreterMonad (Env, RetVal)
-- function definition: add a new function to the environment
interpretStmt (FnDef pos t (Ident ident) args (Block _ stmts)) = do
  env <- ask
  (locMap, nextLoc) <- get
  let env' = M.insert ident nextLoc env
  let store' = M.insert nextLoc (FVal args (Block pos stmts) env') locMap
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
  let maybeLoc = M.lookup ident env
  newVal <- evalExpr expr
  case maybeLoc of
    Nothing -> throwError $ VariableNotDeclared ident $ posToLC pos
    Just loc -> do
      let maybeVal = M.lookup loc locMap
      case maybeVal of
        Nothing -> throwError $ VariableNotDeclared ident $ posToLC pos
        Just _ -> do
          put (M.insert loc newVal locMap, nextLoc)
          return (env, Nothing)

-- incrementation statement:
interpretStmt (Incr pos (Ident ident)) = do
  env <- ask
  (locMap, nextLoc) <- get
  let maybeLoc = M.lookup ident env
  case maybeLoc of
    Just loc -> do
      let maybeVal = M.lookup loc locMap
      case maybeVal of
        Just (Vint n) -> do
          let newVal = n + 1
          put (M.insert loc (Vint newVal) locMap, nextLoc)
          return (env, Nothing)
        _ -> throwError $ Other $ posToLC pos
    _ -> throwError $ Other $ posToLC pos

-- decrementation statement:
interpretStmt (Decr pos (Ident ident)) = do
  env <- ask
  (locMap, nextLoc) <- get
  let maybeLoc = M.lookup ident env
  case maybeLoc of
    Just loc -> do
      let maybeVal = M.lookup loc locMap
      case maybeVal of
        Just (Vint n) -> do
          let newVal = n - 1
          put (M.insert loc (Vint newVal) locMap, nextLoc)
          return (env, Nothing)
        _ -> throwError $ Other $ posToLC pos
    _ -> throwError $ Other $ posToLC pos

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
  let maybeLoc = M.lookup ident env
  case maybeLoc of
    Just loc -> do
      let maybeVal = M.lookup loc locMap
      case maybeVal of
        Just val -> return val
        Nothing -> throwError $ VariableNotDeclared ident $ posToLC pos
    Nothing -> throwError $ VariableNotDeclared ident $ posToLC pos

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

-- TODO: function application
evalExpr (EApp pos (Ident ident) args) = do
  env <- ask
  (locMap, nextLoc) <- get
  loc <- checkMaybe (M.lookup ident env) $ VariableNotDeclared ident $ posToLC pos
  (FVal formalArgs block fEnv) <- checkMaybe (M.lookup loc locMap) $ VariableNotDeclared ident $ posToLC pos

  updatedEnv <- bindArgs pos formalArgs args fEnv
  (_, rV) <- local (const updatedEnv) (interpretStmts [BStmt pos block])
  (locMap, _) <- get
  case rV of
    Just val -> return val
    Nothing -> throwError $ NoReturn $ posToLC pos

-- TODO: lambda application

-- string literal:
evalExpr (EString _ s) = return $ Vstr s
-- arithmetic negation:
evalExpr (Neg pos expr) = do
  val <- evalExpr expr
  case val of
    Vint n -> return $ Vint (-n)
    _ -> throwError $ Other $ posToLC pos

-- logical negation:
evalExpr (Not pos expr) = do
  val <- evalExpr expr
  case val of
    Vbool b -> return $ Vbool (not b)
    _ -> throwError $ Other $ posToLC pos

-- multiplication, division, and modulo:
evalExpr (EMul pos expr1 (Times _) expr2) = do
  val1 <- evalExpr expr1
  val2 <- evalExpr expr2
  case (val1, val2) of
    (Vint n1, Vint n2) -> return $ Vint (n1 * n2)
    _ -> throwError $ Other $ posToLC pos
evalExpr (EMul pos expr1 (Div _) expr2) = do
  val1 <- evalExpr expr1
  val2 <- evalExpr expr2
  case (val1, val2) of
    (Vint n1, Vint 0) -> throwError $ DivisionByZero $ posToLC pos
    (Vint n1, Vint n2) -> return $ Vint (n1 `div` n2)
    _ -> throwError $ Other $ posToLC pos
evalExpr (EMul pos expr1 (Mod _) expr2) = do
  val1 <- evalExpr expr1
  val2 <- evalExpr expr2
  case (val1, val2) of
    (Vint n1, Vint 0) -> throwError $ DivisionByZero $ posToLC pos
    (Vint n1, Vint n2) -> return $ Vint (n1 `mod` n2)
    _ -> throwError $ Other $ posToLC pos

-- addition and subtraction:
evalExpr (EAdd pos expr1 (Plus _) expr2) = do
  val1 <- evalExpr expr1
  val2 <- evalExpr expr2
  case (val1, val2) of
    (Vint n1, Vint n2) -> return $ Vint (n1 + n2)
    _ -> throwError $ Other $ posToLC pos
evalExpr (EAdd pos expr1 (Minus _) expr2) = do
  val1 <- evalExpr expr1
  val2 <- evalExpr expr2
  case (val1, val2) of
    (Vint n1, Vint n2) -> return $ Vint (n1 - n2)
    _ -> throwError $ Other $ posToLC pos

-- relational operators:
evalExpr (ERel pos expr1 op expr2) = do
  val1 <- evalExpr expr1
  val2 <- evalExpr expr2
  case (val1, val2) of
    (Vint n1, Vint n2) -> case op of
      LTH _ -> return $ Vbool (n1 < n2)
      LE _ -> return $ Vbool (n1 <= n2)
      GTH _ -> return $ Vbool (n1 > n2)
      GE _ -> return $ Vbool (n1 >= n2)
      EQU _ -> return $ Vbool (n1 == n2)
      NE _ -> return $ Vbool (n1 /= n2)
    _ -> throwError $ Other $ posToLC pos

-- logical and:
evalExpr (EAnd pos expr1 expr2) = do
  val1 <- evalExpr expr1
  case val1 of
    Vbool False -> return $ Vbool False
    Vbool True -> do
      val2 <- evalExpr expr2
      case val2 of
        Vbool b -> return $ Vbool b
        _ -> throwError $ Other $ posToLC pos
    _ -> throwError $ Other $ posToLC pos

-- logical or:
evalExpr (EOr pos expr1 expr2) = do
  val1 <- evalExpr expr1
  case val1 of
    Vbool True -> return $ Vbool True
    Vbool False -> do
      val2 <- evalExpr expr2
      case val2 of
        Vbool b -> return $ Vbool b
        _ -> throwError $ Other $ posToLC pos
    _ -> throwError $ Other $ posToLC pos

-- TODO: Lambda expression