-- Type checking for MyLatte language
{-# LANGUAGE ImportQualifiedPost #-}

module Types (typeCheck) where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Bool (Bool (True))
import Data.Functor.Classes (eq1)
import Data.List (zipWith)
import Data.Map qualified as M
import Data.Maybe (Maybe, fromMaybe)
import Exception
import MyLatte.Abs
import MyLatte.Abs (ArgType, BNFC'Position)

type Env = M.Map Ident Type

type ReturnType = Maybe Type

type TypeCheckMonad = ReaderT (Env, ReturnType) (ExceptT Exception IO)

-- typeCheckBlock :: Block -> TypeCheckMonad Env
-- typeCheckBlock (Block location statements) = do

typeCheckStatements :: [Stmt] -> TypeCheckMonad (Env, ReturnType)
typeCheckStatements [] = ask
typeCheckStatements (head : tail) = do
  (env, rt) <- typeCheckStatement head
  local (const (env, rt)) (typeCheckStatements tail)

-- exctract FunctionType from function definition:
getFunctionType :: Type -> [Arg] -> Type
-- TODO: function returns a function:
-- getFunctionType (NonReferencable)

-- function returns string, int or bool:
getFunctionType returnType args = do
  let argTypes = map (\(Arg _ argType _) -> argType) args in FunctionType Nothing returnType argTypes

fromArgTypeToType :: ArgType -> Type
fromArgTypeToType (ArgTypeRef _ argType) = argType
fromArgTypeToType (ArgTypeVal _ argType) = argType

-- insert arguments into environment:
insertArgs :: [Arg] -> Env -> Env
insertArgs [] env = env
insertArgs ((Arg _ argType ident) : tail) env = do
  let actualType = fromArgTypeToType argType
  let updatedEnv = M.insert ident actualType env
  insertArgs tail updatedEnv

-- insert items into environment:
insertItems :: [Item] -> Type -> TypeCheckMonad (Env, ReturnType)
insertItems ((NoInit _ ident) : tail) type_ = do
  (env, rt) <- ask
  let updatedEnv = M.insert ident type_ env
  local (const (updatedEnv, rt)) (insertItems tail type_)
insertItems ((Init pos ident expr) : tail) type_ = do
  (env, rt) <- ask
  maybeExprType <- typeCheckExpr expr
  case cmpTypes (Just type_) maybeExprType of
    True -> do
      let updatedEnv = M.insert ident type_ env
      local (const (updatedEnv, rt)) (insertItems tail type_)
    False -> throwError $ BadType (show type_, show maybeExprType) $ posToLC pos
insertItems [] _ = ask

-- Get statements from a block:
getStatements :: Block -> [Stmt]
getStatements (Block _ stmts) = stmts

cmpArgTypes :: [ArgType] -> [ArgType] -> Bool
cmpArgTypes [] [] = True
cmpArgTypes ((ArgTypeRef _ t1) : tail1) ((ArgTypeRef _ t2) : tail2) = t1 == t2 && cmpArgTypes tail1 tail2

dropArgPos :: ArgType -> ArgType
dropArgPos (ArgTypeRef _ t) = ArgTypeRef Nothing (dropPos t)
dropArgPos (ArgTypeVal _ t) = ArgTypeVal Nothing (dropPos t)

dropPos :: Type -> Type
dropPos (Int _) = Int Nothing
dropPos (Str _) = Str Nothing
dropPos (Bool _) = Bool Nothing
dropPos (FunctionType _ retType args) = FunctionType Nothing (dropPos retType) (map dropArgPos args)

cmpTypes :: Maybe Type -> Maybe Type -> Bool
cmpTypes (Just (FunctionType _ t1 args1)) (Just (FunctionType _ t2 args2)) = do
  let cmpArgs = cmpArgTypes args1 args2
  t1 == t2 && cmpArgs
cmpTypes (Just (FunctionType _ _ _)) _ = False
cmpTypes _ (Just (FunctionType _ _ _)) = False
cmpTypes (Just t1) (Just t2) = dropPos t1 == dropPos t2
cmpTypes _ _ = False

typeCheckStatement :: Stmt -> TypeCheckMonad (Env, ReturnType)
-- Function definition: FnDef
typeCheckStatement (FnDef position returnType ident args block) = do
  (env, rt) <- ask
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
  let maybeActualT = M.lookup ident env
  case cmpTypes maybeActualT expectedT of
    True -> return (env, rt)
    False -> throwError $ BadType (show expectedT, show maybeActualT) $ posToLC pos

-- Incrementation: Incr
typeCheckStatement (Incr pos ident) = do
  (env, rt) <- ask
  let maybeActualT = M.lookup ident env
  case maybeActualT of
    Just (Int _) -> return (env, rt)
    _ -> throwError $ BadType ("Int", show maybeActualT) $ posToLC pos

-- Decrementation: Decr
typeCheckStatement (Decr pos ident) = do
  (env, rt) <- ask
  let maybeActualT = M.lookup ident env
  case maybeActualT of
    Just (Int _) -> return (env, rt)
    _ -> throwError $ BadType ("Int", show maybeActualT) $ posToLC pos

-- TODO: Returning value from function: Ret
typeCheckStatement (Ret pos e) = do
  (env, rt) <- ask
  maybeActualRType <- typeCheckExpr e
  case cmpTypes rt maybeActualRType of
    True -> return (env, rt)
    False -> throwError $ BadType (show rt, show maybeActualRType) $ posToLC pos

-- Conditional statement: Cond
typeCheckStatement (Cond pos e1 stmt) = do
  maybeT <- typeCheckExpr e1
  case maybeT of
    Just (Bool _) -> do
      (env, rT) <- ask
      local (const (env, rT)) (typeCheckStatement stmt)
      return (env, rT)
    _ -> throwError $ BadType ("Bool", show maybeT) $ posToLC pos

-- If/else statement: CondElse
typeCheckStatement (CondElse pos e1 s1 s2) = do
  maybeT <- typeCheckExpr e1
  case maybeT of
    Just (Bool _) -> do
      (env, rT) <- ask
      local (const (env, rT)) (typeCheckStatement s1)
      local (const (env, rT)) (typeCheckStatement s2)
      return (env, rT)
    _ -> throwError $ BadType ("Bool", show maybeT) $ posToLC pos

-- While loop: While
typeCheckStatement (While pos e1 stmt) = do
  maybeT <- typeCheckExpr e1

  case maybeT of
    Just (Bool _) -> do
      (env, rT) <- ask
      local (const (env, rT)) (typeCheckStatement stmt)
      return (env, rT)
    _ -> throwError $ BadType ("Bool", show maybeT) $ posToLC pos

-- Single expression: SExp
typeCheckStatement (SExp _ expr) = do
  typeCheckExpr expr
  ask

getArgsFromFun :: Maybe Type -> Maybe [Type]
getArgsFromFun (Just (FunctionType _ _ [])) = Just []
getArgsFromFun (Just (FunctionType _ _ args)) = Just (map fromArgTypeToType args)
getArgsFromFun _ = Nothing

getReturnTypeFromFun :: Maybe Type -> ReturnType
getReturnTypeFromFun (Just (FunctionType _ retType _)) = Just retType
getReturnTypeFromFun _ = Nothing

checkNArgs :: Maybe [Type] -> [Expr] -> TypeCheckMonad (Bool, Int, Int)
checkNArgs (Just args) exprs = do
  let nArgs = length args
  let nExprs = length exprs
  return (nArgs == nExprs, nArgs, nExprs)
checkNArgs Nothing exprs = do
  let nExprs = length exprs
  return (nExprs == 0, 0, nExprs)

-- Expressions:
typeCheckExpr :: Expr -> TypeCheckMonad (Maybe Type)
-- Variable identificator
typeCheckExpr (EVar pos ident) = do
  (env, rt) <- ask
  let maybeType = M.lookup ident env
  return maybeType
-- Literals
typeCheckExpr (ELitInt pos _) = do
  return $ Just (Int pos)
typeCheckExpr (ELitTrue pos) = do
  return $ Just (Bool pos)
typeCheckExpr (ELitFalse pos) = do
  return $ Just (Bool pos)

-- Function applications:
-- "print" function:
-- takes one argument, a value to print (int or string or boolean)
-- returns printed value
typeCheckExpr (EApp pos (Ident "print") argExprs) = do
  (env, rt) <- ask
  -- check number of arguments:
  case 1 == (length argExprs) of
    False -> throwError $ BadNumberOfArguments 1 (length argExprs) $ posToLC pos
    True -> do
      -- check type of argument:
      maybeArgType <- typeCheckExpr (head argExprs)
      case maybeArgType of
        Just (Int _) -> return $ Just (Int pos)
        Just (Str _) -> return $ Just (Str pos)
        Just (Bool _) -> return $ Just (Bool pos)
        _ -> throwError $ BadType ("Int, Str or Bool", show maybeArgType) $ posToLC pos
-- "readInt" function:
-- takes no arguments, returns integer read from stdin
typeCheckExpr (EApp pos (Ident "readInt") []) = do
  return $ Just (Int pos)
typeCheckExpr (EApp pos (Ident "readInt") argExprs) = do
  throwError $ BadNumberOfArguments 0 (length argExprs) $ posToLC pos

-- "readString" function:
-- takes no arguments, returns string read from stdin
typeCheckExpr (EApp pos (Ident "readString") []) = do
  return $ Just (Str pos)
typeCheckExpr (EApp pos (Ident "readString") argExprs) = do
  throwError $ BadNumberOfArguments 0 (length argExprs) $ posToLC pos

-- user defined functions:
typeCheckExpr (EApp pos ident argExprs) = do
  (env, rt) <- ask
  let maybeFType = M.lookup ident env
  -- check if function exists:
  case maybeFType of
    Nothing -> throwError $ FunctionNotDefined (show ident) $ posToLC pos
    Just _ -> do
      let maybeArgs = getArgsFromFun maybeFType
      let maybeReturnType = getReturnTypeFromFun maybeFType
      (nArgsOk, expected, provided) <- checkNArgs maybeArgs argExprs
      case nArgsOk of
        False -> throwError $ BadNumberOfArguments expected provided $ posToLC pos
        True ->
          case maybeArgs of
            Just args -> do
              maybeArgTypes <- mapM typeCheckExpr argExprs
              case maybeArgTypes of
                [] -> do return maybeReturnType
                _ -> do
                  let ok = and (zipWith cmpTypes (map Just args) maybeArgTypes)
                  case ok of
                    True -> return maybeReturnType
                    False -> throwError $ BadType ("Function arguments", show maybeArgTypes) $ posToLC pos

-- TODO: Lambda Applications:

-- String literal:
typeCheckExpr (EString pos _) = do
  return $ Just (Str pos)

-- Arithmetic Negation:
typeCheckExpr (Neg pos expr) = do
  maybeType <- typeCheckExpr expr
  case maybeType of
    Just (Int _) -> return $ Just (Int pos)
    Just _ -> throwError $ BadType ("Int", show maybeType) $ posToLC pos
    Nothing -> throwError $ Other $ posToLC pos

-- Logical Negation:
typeCheckExpr (Not pos exp) = do
  maybeType <- typeCheckExpr exp
  case maybeType of
    Just (Str pos) -> throwError $ BadType ("Bool or Int", "String") $ posToLC pos
    Nothing -> throwError $ Other $ posToLC pos

-- Arithmetic Multiplication:
typeCheckExpr (EMul pos exp1 _ exp2) = do
  maybeType1 <- typeCheckExpr exp1
  maybeType2 <- typeCheckExpr exp2
  case (maybeType1, maybeType2) of
    (Just (Int _), Just (Int _)) -> return $ Just (Int pos)
    _ -> throwError $ BadType ("Int", show maybeType1 ++ " and " ++ show maybeType2) $ posToLC pos

-- Arithmetic Addition:
typeCheckExpr (EAdd pos exp1 _ exp2) = do
  maybeType1 <- typeCheckExpr exp1
  maybeType2 <- typeCheckExpr exp2
  case (maybeType1, maybeType2) of
    (Just (Int _), Just (Int _)) -> return $ Just (Int pos)
    _ -> throwError $ BadType ("Int", show maybeType1 ++ " and " ++ show maybeType2) $ posToLC pos

-- Relations:
typeCheckExpr (ERel pos exp1 _ exp2) = do
  maybeType1 <- typeCheckExpr exp1
  maybeType2 <- typeCheckExpr exp2
  case (maybeType1, maybeType2) of
    (Just (Int _), Just (Int _)) -> return $ Just (Bool pos)
    _ -> throwError $ BadType ("Int", show maybeType1 ++ " and " ++ show maybeType2) $ posToLC pos
-- Logical And:
typeCheckExpr (EAnd pos exp1 exp2) = do
  maybeType1 <- typeCheckExpr exp1
  maybeType2 <- typeCheckExpr exp2
  case (maybeType1, maybeType2) of
    (Just (Bool _), Just (Bool _)) -> return $ Just (Bool pos)
    _ -> throwError $ BadType ("Bool", show maybeType1 ++ " and " ++ show maybeType2) $ posToLC pos

-- Logical Or:
typeCheckExpr (EOr pos exp1 exp2) = do
  maybeType1 <- typeCheckExpr exp1
  maybeType2 <- typeCheckExpr exp2
  case (maybeType1, maybeType2) of
    (Just (Bool _), Just (Bool _)) -> return $ Just (Bool pos)
    _ -> throwError $ BadType ("Bool", show maybeType1 ++ " and " ++ show maybeType2) $ posToLC pos

-- TODO: Lambda expressions:

typeCheck :: Program -> IO (Either Exception (Env, ReturnType))
typeCheck (Program _ stmts) = runExceptT (runReaderT (typeCheckStatements stmts) (M.empty, Nothing))