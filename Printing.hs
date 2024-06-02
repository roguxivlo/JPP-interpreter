module Printing where

import MyLatte.Abs

showArg :: Arg -> String
showArg (Arg pos (ArgTypeRef _ t) ident) = "ref " ++ showType t ++ " " ++ show ident
showArg (Arg pos (ArgTypeVal _ t) ident) = showType t ++ " " ++ show ident

showArgType :: ArgType -> String
showArgType (ArgTypeRef pos t) = showType t
showArgType (ArgTypeVal pos t) = showType t

showArgTypes :: [ArgType] -> String
showArgTypes [] = ""
showArgTypes [x] = showArgType x
showArgTypes (x : xs) = showArgType x ++ ", " ++ showArgTypes xs

showType :: Type -> String
showType (FunctionType pos retT argsT) =
  " Function"
    ++ showType retT
    ++ "("
    ++ showArgTypes argsT
    ++ ")"
showType (Int p) = " Int"
showType (Str p) = " Str"
showType (Bool p) = " Bool"
