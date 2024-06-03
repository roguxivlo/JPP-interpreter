-- Type checking for MyLatte language
{-# LANGUAGE ImportQualifiedPost #-}

module Types (typeCheck) where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Data.Bool (Bool (True))
import Data.List (zipWith)
import Data.Map qualified as M
import Data.Maybe (Maybe, fromMaybe)
import Exception
import MyLatte.Abs
import Typedefs
import Utils

-- nie zawsze local? dla deklaracji tak, a reszta nie
typeCheckStatements :: [Stmt] -> TypeCheckMonad (Env Type, ReturnType)
typeCheckStatements [] = ask
typeCheckStatements (head : tail) = do
  (env, rt) <- typeCheckStatement head
  local (const (env, rt)) (typeCheckStatements tail)

-- insert arguments into environment:

-- insert items into environment:
insertItems :: [Item] -> Type -> TypeCheckMonad (Env Type, ReturnType)
insertItems ((NoInit _ ident) : tail) type_ = do
  (env, rt) <- ask
  let updatedEnv = M.insert ident type_ env
  local (const (updatedEnv, rt)) (insertItems tail type_)
insertItems ((Init pos ident expr) : tail) type_ = do
  (env, rt) <- ask
  t1 <- typeCheckExpr expr
  case cmpTypes (Just type_) (Just t1) of
    True -> do
      let updatedEnv = M.insert ident type_ env
      local (const (updatedEnv, rt)) (insertItems tail type_)
    False -> throwError $ BadType (type_, t1) pos
insertItems [] _ = ask

typeCheckStatement :: Stmt -> TypeCheckMonad (Env Type, ReturnType)
-- Function definition: FnDef
typeCheckStatement (FnDef position returnType ident args block) = do
  (env, rt) <- ask
  validateFunctionIdent ident position
  checkArgDups args position
  -- arguments cannot have the same name as the function:
  checkArgNames ident args position
  -- insert function into environment:
  let updatedEnv = M.insert ident (getFunctionType returnType args) env
  -- insert arguments into environment:

  let updatedEnvWithArgs = insertArgs args updatedEnv
  let retType = getReturnTypeFromFun (M.lookup ident updatedEnvWithArgs)
  -- check block:
  local (const (updatedEnvWithArgs, retType)) (typeCheckStatements [BStmt position block])
  return (updatedEnv, rt)

-- Block: BStmt
typeCheckStatement (BStmt _ block) = do
  (env, rt) <- ask
  let stms = getStatements block
  local (const (env, rt)) (typeCheckStatements stms)
  return (env, rt)

-- Declaration: Decl
typeCheckStatement (Decl _ type_ items) = do
  updatedEnv <- insertItems items type_
  return updatedEnv

-- Assignment: Ass
typeCheckStatement (Ass pos ident expr) = do
  expectedT <- typeCheckExpr expr
  (env, rt) <- ask
  maybeActualT <- findVarType ident pos
  case cmpTypes (Just maybeActualT) (Just expectedT) of
    True -> return (env, rt)
    False -> throwError $ BadType (expectedT, maybeActualT) pos

-- Incrementation: Incr
-- typeCheckStatement (Incr pos ident) = do
--   (env, rt) <- ask
--   maybeActualT <- findVarType ident pos
--   case maybeActualT of
--     Int _ -> return (env, rt)
--     _ -> throwError $ BadType (Int pos, maybeActualT) pos

-- Decrementation: Decr
-- typeCheckStatement (Decr pos ident) = do
--   (env, rt) <- ask
--   maybeActualT <- findVarType ident pos
--   case maybeActualT of
--     Int _ -> return (env, rt)
--     _ -> throwError $ BadType (Int pos, maybeActualT) pos
typeCheckStatement (Ret pos e) = do
  (env, rt) <- ask
  rtFromMaybe <- checkRetLegal pos
  maybeActualRType <- typeCheckExpr e
  case cmpTypes rt (Just maybeActualRType) of
    True -> return (env, rt)
    False -> do
      throwError $ BadType (rtFromMaybe, maybeActualRType) pos

-- Conditional statement: Cond
typeCheckStatement (Cond pos e1 stmt) = do
  maybeT <- typeCheckExpr e1
  case maybeT of
    Bool _ -> do
      (env, rT) <- ask
      local (const (env, rT)) (typeCheckStatement stmt)
      return (env, rT)
    _ -> throwError $ BadType (Bool pos, maybeT) pos

-- If/else statement: CondElse
typeCheckStatement (CondElse pos e1 s1 s2) = do
  maybeT <- typeCheckExpr e1
  case maybeT of
    Bool _ -> do
      (env, rT) <- ask
      local (const (env, rT)) (typeCheckStatement s1)
      local (const (env, rT)) (typeCheckStatement s2)
      return (env, rT)
    _ -> throwError $ BadType (Bool pos, maybeT) pos

-- While loop: While
typeCheckStatement (While pos e1 stmt) = do
  maybeT <- typeCheckExpr e1

  case maybeT of
    Bool _ -> do
      (env, rT) <- ask
      local (const (env, rT)) (typeCheckStatement stmt)
      return (env, rT)
    _ -> throwError $ BadType (Bool pos, maybeT) pos

-- Single expression: SExp
typeCheckStatement (SExp _ expr) = do
  typeCheckExpr expr
  ask

checkFunctionArgs :: [ArgType] -> [Expr] -> BNFC'Position -> TypeCheckMonad ()
checkFunctionArgs [] [] _ = return ()
checkFunctionArgs (h1 : t1) (h2 : t2) pos = do
  argType <- typeCheckExpr h2
  let expectedType = fromArgTypeToType h1
  case cmpTypes (Just expectedType) (Just argType) of
    True -> checkFunctionArgs t1 t2 pos
    False -> throwError $ BadType (expectedType, argType) pos
  return ()

-- Expressions:
typeCheckExpr :: Expr -> TypeCheckMonad Type
-- Variable identificator
typeCheckExpr (EVar pos ident) = do
  (env, rt) <- ask
  maybeType <- findVarType ident pos
  return maybeType
-- Literals
typeCheckExpr (ELitInt pos _) = do
  return $ Int pos
typeCheckExpr (ELitTrue pos) = do
  return $ Bool pos
typeCheckExpr (ELitFalse pos) = do
  return $ Bool pos

-- Function applications:
-- "print" function:
-- takes one argument, a value to print (int or string or boolean)
-- returns printed value
typeCheckExpr (EApp pos (Ident "print") argExprs) = do
  (env, rt) <- ask
  -- check number of arguments:
  case 1 == (length argExprs) of
    False -> throwError $ BadNumberOfArguments 1 (length argExprs) pos
    True -> do
      -- check type of argument:
      t <- typeCheckExpr (head argExprs)
      case t of
        Int _ -> return $ Int pos
        Str _ -> return $ Str pos
        Bool _ -> return $ Bool pos
        _ -> throwError $ BadPrintType t pos
-- "readInt" function:
-- takes no arguments, returns integer read from stdin
typeCheckExpr (EApp pos (Ident "readInt") []) = do
  return $ Int pos
typeCheckExpr (EApp pos (Ident "readInt") argExprs) = do
  throwError $ BadNumberOfArguments 0 (length argExprs) pos

-- "readString" function:
-- takes no arguments, returns string read from stdin
typeCheckExpr (EApp pos (Ident "readString") []) = do
  return $ Str pos
typeCheckExpr (EApp pos (Ident "readString") argExprs) = do
  throwError $ BadNumberOfArguments 0 (length argExprs) pos

-- user defined functions:
typeCheckExpr (EApp pos ident argExprs) = do
  (rT, argsT) <- findFType ident pos
  -- check number of arguments
  checkNArgs argsT argExprs pos
  -- then if ok, check if types of function arguments match
  -- the types provided in argExprs.
  checkFunctionArgs argsT argExprs pos
  return rT

-- String literal:
typeCheckExpr (EString pos _) = do
  return $ Str pos

-- Arithmetic Negation:
typeCheckExpr (Neg pos expr) = do
  t <- typeCheckExpr expr
  case t of
    Int _ -> return $ Int pos
    _ -> throwError $ BadType (Int pos, t) pos

-- Logical Negation:
typeCheckExpr (Not pos exp) = do
  t <- typeCheckExpr exp
  case t of
    (Int _) -> return $ Bool pos
    (Bool _) -> return $ Bool pos
    _ -> throwError $ BadTypeAlternatives [Int pos, Bool pos] t pos

-- Arithmetic Multiplication:
typeCheckExpr (EMul pos exp1 _ exp2) = do
  t1 <- typeCheckExpr exp1
  t2 <- typeCheckExpr exp2
  case (t1, t2) of
    (Int _, Int _) -> return $ Int pos
    (Int _, _) -> throwError $ BadType (Int pos, t2) pos
    _ -> throwError $ BadType (Int pos, t1) pos

-- Arithmetic Addition:
typeCheckExpr (EAdd pos exp1 _ exp2) = do
  t1 <- typeCheckExpr exp1
  t2 <- typeCheckExpr exp2
  case (t1, t2) of
    (Int _, Int _) -> return $ Int pos
    (Int _, _) -> throwError $ BadType (Int pos, t2) pos
    _ -> throwError $ BadType (Int pos, t1) pos

-- Relations and Comparisons:
typeCheckExpr (ERel pos exp1 _ exp2) = do
  t1 <- typeCheckExpr exp1
  t2 <- typeCheckExpr exp2
  case (t1, t2) of
    (Int _, Int _) -> return $ Bool pos
    (Str _, Str _) -> return $ Bool pos
    (Bool _, Bool _) -> return $ Bool pos
    (Int _, _) -> throwError $ BadType (Int pos, t2) pos
    (Str _, _) -> throwError $ BadType (Str pos, t2) pos
    (Bool _, _) -> throwError $ BadType (Bool pos, t2) pos
    _ -> throwError $ BadTypeAlternatives [Int pos, Str pos, Bool pos] t1 pos

-- Logical And:
typeCheckExpr (EAnd pos exp1 exp2) = do
  t1 <- typeCheckExpr exp1
  t2 <- typeCheckExpr exp2
  case (t1, t2) of
    (Bool _, Bool _) -> return $ Bool pos
    (Bool _, _) -> throwError $ BadType (Bool pos, t2) pos
    _ -> throwError $ BadType (Bool pos, t1) pos

-- Logical Or:
typeCheckExpr (EOr pos exp1 exp2) = do
  t1 <- typeCheckExpr exp1
  t2 <- typeCheckExpr exp2
  case (t1, t2) of
    (Bool _, Bool _) -> return $ Bool pos
    (Bool _, _) -> throwError $ BadType (Bool pos, t2) pos
    _ -> throwError $ BadType (Bool pos, t1) pos

-- Concatenation:
typeCheckExpr (Concat pos exp1 exp2) = do
  t1 <- typeCheckExpr exp1
  t2 <- typeCheckExpr exp2
  case (t1, t2) of
    (Str _, Str _) -> return $ Str pos
    (Str _, _) -> throwError $ BadType (Str pos, t2) pos
    _ -> throwError $ BadType (Str pos, t1) pos

-- TODO: Lambda expressions:
typeCheckExpr (LExpr pos retType args block) = do
  (env, rt) <- ask
  checkArgDups args pos
  let updatedEnv = insertArgs args env
  local (const (updatedEnv, Just retType)) (typeCheckStatements [BStmt pos block])
  return $ getFunctionType retType args
typeCheckExpr (LApp pos rType args block exprs) = do
  (env, rt) <- ask
  let argTypes = map argToArgType args
  checkArgDups args pos
  checkNArgs argTypes exprs pos
  checkFunctionArgs argTypes exprs pos
  let updatedEnv = insertArgs args env
  local (const (updatedEnv, Just rType)) (typeCheckStatements [BStmt pos block])
  return rType

typeCheck :: Program -> Either TypeCheckErr (Env Type, ReturnType)
typeCheck (Program _ stmts) = runIdentity (runExceptT (runReaderT (typeCheckStatements stmts) (M.empty, Nothing)))