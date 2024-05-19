module Exception where

import GHC.TypeLits (Div)
import MyLatte.Abs

type Line = Int

type Col = Int

type ExpectedType = String

type ActualType = String

type VarName = String

posToLC :: BNFC'Position -> (Line, Col)
posToLC (Just (l, c)) = (l, c)
posToLC Nothing = (0, 0)

data Exception
  = BadType (ExpectedType, ActualType) (Line, Col)
  | VariableNotDeclared VarName (Line, Col)
  | BadNumberOfArguments Int Int (Line, Col)
  | FunctionNotDefined VarName (Line, Col)
  | DivisionByZero (Line, Col)
  | InvalidPassByReference (Line, Col)
  | NoReturn (Line, Col)
  | Other (Line, Col)

instance Show Exception where
  show (BadType (expected, actual) (line, col)) = "Bad type: expected " ++ expected ++ ", got " ++ actual ++ " at line " ++ show line ++ ", column " ++ show col
  show (VariableNotDeclared name (line, col)) = "Variable " ++ name ++ " at line " ++ show line ++ " column " ++ show col ++ " not declared."
  show (BadNumberOfArguments expected actual (line, col)) = "Bad number of arguments: expected " ++ show expected ++ ", got " ++ show actual ++ " at line " ++ show line ++ ", column " ++ show col
  show (Other (line, col)) = "Error at line " ++ show line ++ ", column " ++ show col
  show (FunctionNotDefined name (line, col)) = "Function " ++ name ++ " at line " ++ show line ++ " column " ++ show col ++ " not defined."
  show (DivisionByZero (line, col)) = "Division by zero at line " ++ show line ++ ", column " ++ show col
  show (InvalidPassByReference (line, col)) = "Invalid pass by reference at line " ++ show line ++ ", column " ++ show col
  show (NoReturn (line, col)) = "Function returned nothing at line " ++ show line ++ ", column " ++ show col