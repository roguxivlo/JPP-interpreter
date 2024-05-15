module Exception where

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
  | Other (Line, Col)

instance Show Exception where
  show (BadType (expected, actual) (line, col)) = "Bad type: expected " ++ expected ++ ", got " ++ actual ++ " at line " ++ show line ++ ", column " ++ show col
  show (VariableNotDeclared name (line, col)) = "Variable " ++ name ++ " at line " ++ show line ++ " column " ++ show col ++ " not declared."