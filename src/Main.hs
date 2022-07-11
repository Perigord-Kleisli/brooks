module Main where

import Parser
import Compiler
import Text.Printf
import Data.Function
import Data.Char
import Control.Monad.State

showLambda :: Lambda -> String
showLambda (Identifier x) = x
showLambda (Application x y) = (printf "%s %s" `on` showLambda) x y
showLambda (Abstraction x y) = (printf "λ%s.%s" `on` showLambda) x y
showLambda (Grouping x) = printf "(%s)" $ showLambda x

showTree :: Lambda -> String
showTree = go 0
  where
    spaces x y = mappend "\n" (concat $ replicate x "  ") <> y
    go :: Int -> Lambda -> String
    go n (Identifier x) = spaces n $ printf "(Var %s)" x
    go n (Grouping x) = spaces n $ printf "(Group %s)" (go (n+1) x)
    go n (Application x y) = spaces n $ 
      (printf "(App %s %s)" `on` go (n+1)) x y
    go n (Abstraction x y) = spaces n $ 
      (printf "(Abs %s %s)" `on` go (n+1)) x y

printLambda :: String -> IO ()
printLambda = print . either (const "") showLambda . parser

unsafeRun :: String -> IO()
unsafeRun x = print $ ($0) $ either undefined id $  either undefined id $ either undefined id . (`evalStateT` mempty) . compiler id <$> parser x  

main :: IO ()
main = print "Hello World"
