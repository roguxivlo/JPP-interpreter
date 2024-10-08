module Exception where

import MyLatte.Abs
import Printing

type Line = Int

type Col = Int

type ExpectedType = Type

type ActualType = Type

type VarName = String

posToLC :: BNFC'Position -> (Line, Col)
posToLC (Just (l, c)) = (l, c)
posToLC Nothing = (-1, -1)

data TypeCheckErr
  = BadType (ExpectedType, ActualType) BNFC'Position
  | BadTypeAlternatives [ExpectedType] ActualType BNFC'Position
  | VariableNotDeclared VarName BNFC'Position
  | BadNumberOfArguments Int Int BNFC'Position
  | FunctionNotDefined VarName BNFC'Position
  | ArgNameRepeated VarName BNFC'Position
  | IllegalIdent VarName BNFC'Position
  | ArgNameIsFunName BNFC'Position
  | BadPrintType ActualType BNFC'Position
  | IllegalReturn BNFC'Position

data RuntimeErr
  = DivisionByZero BNFC'Position
  | InvalidPassByReference BNFC'Position
  | NoReturn BNFC'Position
  | Other BNFC'Position

typeCheckMsg = "Type Checker Error: "

runtimeMsg = "Runtime Error: "

showBNFC'Position :: BNFC'Position -> String
showBNFC'Position pos =
  let (l, c) = posToLC pos in " at line " ++ show l ++ ", column " ++ show c ++ " "

instance Show TypeCheckErr where
  show (BadType (e, a) pos) =
    typeCheckMsg
      ++ "Bad type: expected "
      ++ showType e
      ++ ", got "
      ++ showType a
      ++ showBNFC'Position pos
  show (VariableNotDeclared name pos) =
    typeCheckMsg
      ++ "Variable "
      ++ name
      ++ showBNFC'Position pos
      ++ " not declared."
  show (BadNumberOfArguments e a pos) =
    typeCheckMsg
      ++ "Bad number of arguments: expected "
      ++ show e
      ++ ", got "
      ++ show a
      ++ showBNFC'Position pos
  show (FunctionNotDefined name pos) =
    typeCheckMsg
      ++ "Function "
      ++ name
      ++ showBNFC'Position pos
      ++ " not defined."
  show (BadPrintType a pos) =
    typeCheckMsg
      ++ "Bad Print Argument Type: Expected Int, Str or Bool, got "
      ++ showType a
      ++ showBNFC'Position pos
  show (BadTypeAlternatives elist a pos) =
    typeCheckMsg
      ++ "Bad type: expected one of "
      ++ foldl (\acc elt -> acc ++ ", " ++ showType elt) "" elist
      ++ ", got "
      ++ showType a
      ++ showBNFC'Position pos
  show (ArgNameRepeated name pos) =
    typeCheckMsg
      ++ "Argument name "
      ++ name
      ++ showBNFC'Position pos
      ++ " repeated."
  show (IllegalIdent name pos) =
    typeCheckMsg
      ++ "Illegal identifier "
      ++ name
      ++ showBNFC'Position pos
  show (ArgNameIsFunName pos) =
    typeCheckMsg
      ++ "Argument name is the same as function name"
      ++ showBNFC'Position pos
  show (IllegalReturn pos) =
    typeCheckMsg ++ "Illegal return" ++ showBNFC'Position pos

instance Show RuntimeErr where
  show (Other pos) =
    runtimeMsg ++ "Error" ++ showBNFC'Position pos
  show (InvalidPassByReference pos) =
    runtimeMsg ++ "Invalid pass by reference" ++ showBNFC'Position pos
  show (DivisionByZero pos) =
    runtimeMsg ++ "Division by zero" ++ showBNFC'Position pos
  show (NoReturn pos) =
    runtimeMsg ++ "Function returned nothing" ++ showBNFC'Position pos
