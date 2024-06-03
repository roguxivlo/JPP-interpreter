module Utils where

-- fromMaybe

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Exception
import MyLatte.Abs
import Typedefs

findVarType :: Ident -> BNFC'Position -> TypeCheckMonad Type
findVarType (Ident ident) pos = do
  (env, _) <- ask
  let maybeType = M.lookup (Ident ident) env
  case maybeType of
    Just t -> return t
    Nothing -> throwError $ VariableNotDeclared (show ident) pos

findVarLoc :: Ident -> BNFC'Position -> InterpreterMonad Loc
findVarLoc (Ident ident) pos = do
  env <- ask
  let maybeLoc = M.lookup (Ident ident) env
  return $ fromMaybe (error "Variable not declared") maybeLoc

findVal :: Loc -> BNFC'Position -> InterpreterMonad Val
findVal loc pos = do
  (locMap, _) <- get
  let maybeVal = M.lookup loc locMap
  return $ fromMaybe (error "Variable not declared") maybeVal

checkRetLegal :: BNFC'Position -> TypeCheckMonad Type
checkRetLegal pos = do
  (_, rt) <- ask
  case rt of
    Just t -> return t
    Nothing -> throwError $ IllegalReturn pos

-- exctract FunctionType from function definition:
getFunctionType :: Type -> [Arg] -> Type
-- TODO: function returns a function:
-- getFunctionType (NonReferencable)

-- function returns string, int or bool:
getFunctionType returnType args = do
  let argTypes = map (\(Arg _ argType _) -> argType) args
   in FunctionType Nothing returnType argTypes

fromArgTypeToType :: ArgType -> Type
fromArgTypeToType (ArgTypeRef _ argType) = argType
fromArgTypeToType (ArgTypeVal _ argType) = argType

-- insert arguments into environment:
insertArgs :: [Arg] -> Env Type -> Env Type
insertArgs [] env = env
insertArgs ((Arg _ argType ident) : tail) env = do
  let actualType = fromArgTypeToType argType
  let updatedEnv = M.insert ident actualType env
  insertArgs tail updatedEnv

-- Get statements from a block:
getStatements :: Block -> [Stmt]
getStatements (Block _ stmts) = stmts

cmpArgTypes :: [ArgType] -> [ArgType] -> Bool
cmpArgTypes [] [] = True
cmpArgTypes ((ArgTypeRef _ t1) : tail1) ((ArgTypeRef _ t2) : tail2) = cmpTypes (Just t1) (Just t2) && cmpArgTypes tail1 tail2
cmpArgTypes ((ArgTypeVal _ t1) : tail1) ((ArgTypeVal _ t2) : tail2) = cmpTypes (Just t1) (Just t2) && cmpArgTypes tail1 tail2
cmpArgTypes _ _ = False

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
  cmpTypes (Just t1) (Just t2) && cmpArgs
cmpTypes (Just (FunctionType _ _ _)) _ = False
cmpTypes _ (Just (FunctionType _ _ _)) = False
cmpTypes (Just t1) (Just t2) = dropPos t1 == dropPos t2
cmpTypes _ _ = False

getArgsFromFun :: Maybe Type -> Maybe [Type]
getArgsFromFun (Just (FunctionType _ _ [])) = Just []
getArgsFromFun (Just (FunctionType _ _ args)) = Just (map fromArgTypeToType args)
getArgsFromFun _ = Nothing

getReturnTypeFromFun :: Maybe Type -> ReturnType
getReturnTypeFromFun (Just (FunctionType _ retType _)) = Just retType
getReturnTypeFromFun _ = Nothing

findFType :: Ident -> BNFC'Position -> TypeCheckMonad (Type, [ArgType])
findFType (Ident ident) pos = do
  (env, rT) <- ask
  let maybeFType = M.lookup (Ident ident) env
  case maybeFType of
    Just (FunctionType pos rType argsT) -> return (rType, argsT)
    _ -> throwError $ FunctionNotDefined (show ident) pos

argToArgType :: Arg -> ArgType
argToArgType (Arg pos argType ident) = argType

checkNArgs :: [ArgType] -> [Expr] -> BNFC'Position -> TypeCheckMonad ()
checkNArgs args exprs pos = do
  let nActual = length exprs
  let nExpected = length args
  case nActual == nExpected of
    True -> return ()
    False -> throwError $ BadNumberOfArguments nExpected nActual pos

idToStr :: Ident -> String
idToStr (Ident s) = s

getArgName :: Arg -> String
getArgName (Arg _ _ ident) = idToStr ident

-- find first duplicate in a list:
findDup :: (Eq a) => [a] -> Maybe a
findDup [] = Nothing
findDup (x : xs) = case elem x xs of
  True -> Just x
  False -> findDup xs

checkArgDups :: [Arg] -> BNFC'Position -> TypeCheckMonad ()
checkArgDups args pos = do
  let argNames = map getArgName args
  case findDup argNames of
    Just dup -> throwError $ ArgNameRepeated dup pos
    Nothing -> return ()

checkArgNames :: Ident -> [Arg] -> BNFC'Position -> TypeCheckMonad ()
checkArgNames ident args pos = do
  let argNames = map getArgName args
  case elem (idToStr ident) argNames of
    True -> throwError $ ArgNameIsFunName pos
    False -> return ()

validateFunctionIdent :: Ident -> BNFC'Position -> TypeCheckMonad ()
validateFunctionIdent (Ident ident) pos = do
  case ident of
    "print" -> throwError $ IllegalIdent ident pos
    "readInt" -> throwError $ IllegalIdent ident pos
    "readString" -> throwError $ IllegalIdent ident pos
    _ -> return ()