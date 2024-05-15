module Types where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Bool (Bool (True))
import Data.Functor.Classes (eq1)
import Data.List (zipWith)
import Data.Map qualified as M
import Data.Maybe (Maybe, fromMaybe)
import Exception
import Foreign.C (eNODEV)
import GHC.IOPort (newEmptyIOPort)
import GHC.ResponseFile (expandResponse)
import MyLatte.Abs
import MyLatte.Abs (ArgType, BNFC'Position)
import Text.ParserCombinators.ReadP (get)

type Env = M.Map Ident Type

type TypeCheckMonad = ReaderT Env (ExceptT Exception IO)

-- typeCheckBlock :: Block -> TypeCheckMonad Env
-- typeCheckBlock (Block location statements) = do

typeCheckStatements :: [Stmt] -> TypeCheckMonad Env
typeCheckStatements [] = ask
typeCheckStatements (head : tail) = do
  env <- typeCheckStatement head
  local (const env) (typeCheckStatements tail)

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
insertItems :: [Item] -> Type -> Env -> Env
insertItems ((NoInit _ ident) : tail) type_ env = do
  let updatedEnv = M.insert ident type_ env
  insertItems tail type_ updatedEnv
insertItems ((Init _ ident expr) : tail) type_ env = do
  let updatedEnv = M.insert ident type_ env
  insertItems tail type_ updatedEnv
insertItems [] _ env = env

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

typeCheckStatement :: Stmt -> TypeCheckMonad Env
-- Function definition: FnDef
typeCheckStatement (FnDef position returnType ident args block) = do
  env <- ask
  -- insert function into environment:
  let updatedEnv = M.insert ident (getFunctionType returnType args) env
  -- insert arguments into environment:
  let updatedEnvWithArgs = insertArgs args updatedEnv
  -- check block:
  local (const updatedEnvWithArgs) (typeCheckStatements [BStmt position block])
  return updatedEnv

-- Block: BStmt
typeCheckStatement (BStmt _ block) = do
  env <- ask
  let stms = getStatements block
  local (const env) (typeCheckStatements stms)

-- Declaration: Decl
-- FIX!
typeCheckStatement (Decl _ type_ items) = do
  env <- ask
  let updatedEnv = insertItems items type_ env
  return updatedEnv

-- Assignment: Ass
typeCheckStatement (Ass pos ident expr) = do
  expectedT <- typeCheckExpr expr
  env <- ask
  let maybeActualT = M.lookup ident env
  case cmpTypes maybeActualT expectedT of
    True -> return env
    False -> throwError $ BadType (show expectedT, show maybeActualT) $ posToLC pos

-- Incrementation: Incr
typeCheckStatement (Incr pos ident) = do
  env <- ask
  let maybeActualT = M.lookup ident env
  case maybeActualT of
    Just (Int _) -> return env
    _ -> throwError $ BadType ("Int", show maybeActualT) $ posToLC pos

-- Decrementation: Decr
typeCheckStatement (Decr pos ident) = do
  env <- ask
  let maybeActualT = M.lookup ident env
  case maybeActualT of
    Just (Int _) -> return env
    _ -> throwError $ BadType ("Int", show maybeActualT) $ posToLC pos

-- TODO: Returning value from function: Ret
typeCheckStatement (Ret pos e) = ask
-- Conditional statement: Cond
typeCheckStatement (Cond pos e1 stmt) = do
  maybeT <- typeCheckExpr e1
  case maybeT of
    Just (Bool _) -> typeCheckStatement stmt
    _ -> throwError $ BadType ("Bool", show maybeT) $ posToLC pos

-- If/else statement: CondElse
typeCheckStatement (CondElse pos e1 s1 s2) = do
  maybeT <- typeCheckExpr e1
  case maybeT of
    Just (Bool _) -> do
      typeCheckStatement s1
      typeCheckStatement s2
    _ -> throwError $ BadType ("Bool", show maybeT) $ posToLC pos

-- While loop: While
typeCheckStatement (While pos e1 stmt) = do
  maybeT <- typeCheckExpr e1
  case maybeT of
    Just (Bool _) -> typeCheckStatement stmt
    _ -> throwError $ BadType ("Bool", show maybeT) $ posToLC pos

-- Single expression: SExp
typeCheckStatement (SExp _ expr) = do
  typeCheckExpr expr
  ask

getArgsFromFun :: Maybe Type -> Maybe [Type]
getArgsFromFun (Just (FunctionType _ _ [])) = Just []
getArgsFromFun (Just (FunctionType _ _ args)) = Just (map fromArgTypeToType args)
getArgsFromFun _ = Nothing

getReturnTypeFromFun :: Maybe Type -> Maybe Type
getReturnTypeFromFun (Just (FunctionType _ retType _)) = Just retType
getReturnTypeFromFun _ = Nothing

-- Expressions:
typeCheckExpr :: Expr -> TypeCheckMonad (Maybe Type)
-- Variable identificator
typeCheckExpr (EVar pos ident) = do
  env <- ask
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
typeCheckExpr (EApp pos ident argExprs) = do
  env <- ask
  let maybeFType = M.lookup ident env
  let maybeArgs = getArgsFromFun maybeFType
  let maybeReturnType = getReturnTypeFromFun maybeFType
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
  case cmpTypes maybeType1 maybeType2 of
    True -> return $ Just (Bool pos)
    False -> throwError $ BadType ("Types have to be the same", show maybeType1 ++ " and " ++ show maybeType2) $ posToLC pos

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

typeCheck :: Program -> IO (Either Exception Env)
typeCheck (Program _ stmts) = runExceptT (runReaderT (typeCheckStatements stmts) M.empty)