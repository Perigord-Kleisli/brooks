{-# LANGUAGE LambdaCase #-}
module Eval where

import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map.Strict as M

import Control.Arrow
import Data.Function
import Text.Printf

import Parser

data Val = Num Int | Func String Lambda
  deriving Show

type Env = M.Map String Val
eval :: Lambda -> ExceptT String (State Env) Val

eval (Number x) = return $ Num x
eval (Identifier i)  = get >>= (maybe (throwError ("Unknown Variable '" <> i <> "'")) return . M.lookup i)

eval (x :+: y) = (liftM2 oper `on` eval) x y
  where
    oper (Num a) (Num b) = Num $ a + b
    oper _ _ = undefined

eval (Grouping x) = eval x
eval (Abstraction arg expr) = return $ Func arg expr
eval (Application f' arg') = do
  (i,expr) <- (\case (Func i expr) -> return (i,expr)
                     _             -> throwError $ printf "argument '%s' applied to non-function '%s'" (showLambda f') (showLambda arg'))
                =<< eval f'
  arg <- eval arg'
  modify (M.insert i arg)
  eval expr


stdLib :: M.Map String Val
stdLib = M.fromList [("I", Func "x" (Identifier "x"))]

betaReduction :: Lambda -> String
betaReduction =
  either ("Error: " <>) id
    . (`evalState` stdLib)
    . runExceptT
    . fmap (showLambda . funcToAbs)
    . eval
    . alphaConvert
    . unappendAdd 
  where funcToAbs ~(Func x body) = Abstraction x body

runLambda :: String -> (a -> a) -> Either String (a -> a)
runLambda lambda f = do
  n <- left (replace . show) (parser lambda) 
      >>= (`evalState` stdLib)
          . runExceptT
          . eval
      >>= (\case (Num n) -> Right n
                 x -> throwError $ printf "Unable to reduce '%s' any further" (showLambda $ funcToAbs x))
  return $ if n > 0 then foldr1 (.) $ replicate n f else id
  where 
    funcToAbs ~(Func x body) = Abstraction x body

    -- The show function of `ParseError` tends to escape unicode characters and '\'
    -- Whilst inefficient, this ensures '\955' turn to 'λ' and '\\' to '\'
    replace ('\\' : '9': '5': '5' :xs) = 'λ' : replace xs
    replace ('\\' : '\\' : xs) = '\\' : replace xs
    replace (x:xs)       = x : replace xs
    replace ""           = ""
