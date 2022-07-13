module Main where

import Parser
import Compiler
import Text.Printf
import Data.Function
import Data.Char
import Control.Monad.State
import VM
import qualified Data.Map.Strict as M
import System.Environment
import Control.Arrow

showLambda :: Lambda -> String
showLambda (Identifier x) = x
showLambda (Number x) = show x
showLambda (x :+: y) = (printf "(+ %s %s)" `on` showLambda) x y
showLambda (Application x y) = (printf "%s %s" `on` showLambda) x y
showLambda (Abstraction x y) = (printf "λ%s.%s" `on` showLambda) (Identifier x) y
showLambda (Grouping x) = printf "(%s)" $ showLambda x

showTree :: Lambda -> String
showTree = go 0
  where
    spaces x y = mappend "\n" (concat $ replicate x "  ") <> y
    go :: Int -> Lambda -> String
    go n (Number x) = spaces n $ printf "(Var %s)" $ show x
    go n (x :+: y) = spaces n $ (printf "(+ %s %s)" `on` go (n+1)) x y
    go n (Identifier x) = spaces n $ printf "(Var %s)" x
    go n (Grouping x) = spaces n $ printf "(Group %s)" (go (n+1) x)
    go n (Application x y) = spaces n $ 
      (printf "(App %s %s)" `on` go (n+1)) x y
    go n (Abstraction x y) = spaces n $ 
      (printf "(Abs %s %s)" `on` go (n+1)) (Identifier x) y

printLambda, printTree :: String -> IO ()
printLambda = putStrLn . either (const "") showLambda . parser
printTree = putStrLn . either (const "") showTree . parser


runLambda :: String -> (a -> a) -> Either String (a -> a)
runLambda lambda f = do
  ~(Num x) <- show +++ ((`evalState` stdLib) . eval) $ parser lambda
  return $ foldr1 (.) $ replicate x f
  where
    stdLib = M.fromList 
      [("I", Func "x" (Identifier "x") )]

main :: IO ()
main = getArgs 
       >>= mapM (fmap (`runLambda` (+1)) . readFile) 
       >>= mapM_ (either fail (print . ($ 1)))
