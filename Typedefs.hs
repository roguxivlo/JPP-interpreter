module Typedefs where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Data.Map qualified as M
import Data.Maybe
import Exception
import MyLatte.Abs
import Printing

type Env a = M.Map Ident a

data Val = Vint Integer | Vbool Bool | Vstr String | FVal Type [Arg] Block (Env Loc)

instance Show Val where
  show (Vint n) = show n
  show (Vbool b) = show b
  show (Vstr s) = s
  show (FVal rType args block env) = "Function: " ++ show args ++ " -> " ++ showType rType ++ show env

instance Eq Val where
  (Vint n) == (Vint m) = n == m
  (Vbool b) == (Vbool c) = b == c
  (Vstr s) == (Vstr t) = s == t
  _ == _ = False

instance Ord Val where
  (Vint n) <= (Vint m) = n <= m
  (Vbool b) <= (Vbool c) = b <= c
  (Vstr s) <= (Vstr t) = s <= t
  _ <= _ = False

type Loc = Integer

type Name = String

type RetVal = Maybe Val

type ReturnType = Maybe Type

type LocMap = M.Map Loc Val

type Store = (LocMap, Loc)

type TypeCheckMonad = ReaderT (Env Type, ReturnType) (ExceptT TypeCheckErr Identity)

type InterpreterMonad = ReaderT (Env Loc) (StateT Store (ExceptT RuntimeErr IO))