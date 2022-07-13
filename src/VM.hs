module VM where

import qualified Data.Map.Strict as M
import Control.Monad.State

import Parser
import Data.Maybe
import Data.Function

data Val = Num Int | Func String Lambda
  deriving Show

type Env = M.Map String Val

eval :: Lambda -> State Env Val

eval (Number x) = return $ Num x
eval (Identifier i)  = 
  gets (fromMaybe (error $ "Unknown Variable '" ++ i ++ "'") . M.lookup i)

eval (x :+: y) = (liftM2 oper `on` eval) x y
  where
    oper ~(Num x) ~(Num y) = Num $ x + y

eval (Grouping x) = eval x
eval (Abstraction arg expr) = return $ Func arg expr
eval (Application f' arg') = do
  ~(Func i expr) <- eval f'
  arg <- eval arg'
  modify (M.insert i arg)
  eval expr


