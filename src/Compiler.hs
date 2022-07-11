module Compiler where

import Parser
import qualified Data.Map.Strict as M
import Control.Monad.State
import Data.Maybe

type LambdaF a = Either Lambda (a -> a)
type VarTable a = M.Map String [LambdaF a]

undefinedVar :: String -> a
undefinedVar x = error ("Undefined Variable '" <> x <> "'")

compiler :: (a -> a) -> Lambda -> StateT (VarTable a) (Either InterpreterError) (LambdaF a)

compiler _ (Identifier x) = do 
  var <- gets (maybe (undefinedVar x) head . M.lookup x)
  modify $ M.update tail' x
  return var
  where
    tail' (x:xs) = Just xs
    tail' [] = Nothing

compiler f (Abstraction (Identifier arg) body) = do 
  modify (M.insertWith (++) arg [Right f]) 
  undefined

--compiler :: Lambda -> (a -> a) -> Either String (a -> a)
--compiler lambda f = undefined
--

