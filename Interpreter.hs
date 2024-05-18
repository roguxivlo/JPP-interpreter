module Interpreter where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Map qualified as M
import Exception
import Exception (Exception (Other, VariableNotDeclared), posToLC)
import GHC.Show (Show)
import MyLatte.Abs
import MyLatte.Abs (Expr)

data Val = Vint Integer | Vbool Bool | Vstr String

instance Show Val where
  show (Vint n) = show n
  show (Vbool b) = show b
  show (Vstr s) = s

type Loc = Integer

type Name = String

type Env = M.Map Name Loc

type LocMap = M.Map Loc Val

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
  env <- ask
  (locMap, nextLoc) <- get
  val <- evalExpr expr
  put (M.insert nextLoc val locMap, nextLoc + 1)
  let updatedEnv = M.insert ident nextLoc env
  return (updatedEnv, Nothing)
-- uninitialized variable has to be initialized before use
insertItems t ((NoInit pos (Ident ident)) : tail) = do
  env <- ask
  (locMap, nextLoc) <- get
  updatedLocMap <- case t of
    Int _ -> return $ M.insert nextLoc (Vint 0) locMap
    Bool _ -> return $ M.insert nextLoc (Vbool False) locMap
    Str _ -> return $ M.insert nextLoc (Vstr "") locMap

  put (locMap, nextLoc + 1)
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
-- empty statement
interpretStmt (Empty _) = do
  env <- ask
  return (env, Nothing)

-- block statement
interpretStmt (BStmt _ (Block _ stmts)) = interpretStmts stmts
-- variable declaration: add a new variable to the environment
interpretStmt (Decl pos t items) = do
  -- print debug info
  let x = print "Decl"
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
-- TODO: function application
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