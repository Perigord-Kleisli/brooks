module Main where

import           Control.Arrow
import           Control.Monad.State
import           Data.Function
import qualified Data.Map.Strict               as M
import           System.Console.GetOpt
import           System.Environment
import           Text.Printf

import           Parser
import           System.IO
import           VM

showLambda :: Lambda -> String
showLambda (Identifier x     ) = x
showLambda (Number     x     ) = show x
showLambda (x           :+: y) = (printf "(+ %s %s)" `on` showLambda) x y
showLambda (Application x   y) = (printf "%s %s" `on` showLambda) x y
showLambda (Abstraction x   y) = (printf "λ%s.%s" `on` showLambda) (Identifier x) y
showLambda (Grouping x       ) = printf "(%s)" $ showLambda x

showTree :: Lambda -> String
showTree = go 0
 where
  spaces x y = mappend "\n" (concat $ replicate x "  ") <> y
  go :: Int -> Lambda -> String
  go n (Number x       ) = spaces n $ printf "(Var %s)" $ show x
  go n (x :+: y        ) = spaces n $ (printf "(+ %s %s)" `on` go (n + 1)) x y
  go n (Identifier x   ) = spaces n $ printf "(Var %s)" x
  go n (Grouping   x   ) = spaces n $ printf "(Group %s)" (go (n + 1) x)
  go n (Application x y) = spaces n $ (printf "(App %s %s)" `on` go (n + 1)) x y
  go n (Abstraction x y) =
    spaces n $ (printf "(Abs %s %s)" `on` go (n + 1)) (Identifier x) y

printLambda, printTree :: String -> IO ()
printLambda = putStrLn . either (const "") showLambda . parser
printTree = putStrLn . either (const "") showTree . parser


runLambda :: String -> (a -> a) -> Either String (a -> a)
runLambda lambda f = do
  ~(Num x) <- show +++ ((`evalState` stdLib) . eval) $ parser lambda
  return $ foldr1 (.) $ replicate x f
  where stdLib = M.fromList [("I", Func "x" (Identifier "x"))]

data Settings = Settings
  { parseOnly :: Bool
  , reduction :: Bool
  }
  deriving Show

options :: [OptDescr Settings]
options =
  [ Option ""  ["parse-only"] (NoArg (Settings True False)) "Show only the syntax tree"
  , Option "r" ["reduction"]  (NoArg (Settings False True)) "Do a β reduction"
  ]

compileOpts :: [String] -> IO ([Settings], [String])
compileOpts argv = case getOpt Permute options argv of
  (o, n, []  ) -> return (o, n)
  (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: lambda [OPTION...] files..."


main :: IO ()
main = do
  ([Settings parseOnly reduction], files) <-
    getArgs >>= fmap (second $ mapM readFile) . compileOpts >>= sequence
  if parseOnly
    then forM_ files (either (hPrint stderr) (putStrLn . showTree) . parser)
  else if reduction 
    then forM_ files undefined
  else undefined

  undefined
