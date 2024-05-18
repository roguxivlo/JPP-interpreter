module Interpreter where

    import MyLatte.Abs

    import Control.Monad.State
    import Control.Monad.Reader
    import Control.Monad.Except

    import Data.Map qualified as M

    data Val = Vint Integer | Vbool Bool | Vstr String

    type Loc = Integer
    type Name = String

    type Env = M.Map Name Loc

    type LocMap = M.Map Loc Val
    -- Store is a pair of a map from locations to values and the next free location
    type Store = (LocMap, Loc)

    